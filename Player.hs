{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Player where

import           Control.Monad
import           Control.Monad.RWS
import           Data.Array
import           Data.ByteString                ( ByteString )
import           Data.Vector                    ( Vector )
import           Data.Word
import           Debug.Trace
import           Prelude                 hiding ( pi )
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V

import           Constants
import           Types
import           Render

--------------------
-- Definitions
--------------------

-- The .MOD playback monad context.
type Playback m = MonadRWS Module [ByteString] PlayerState m

-- A sound being played on a channel.
data Sound =
    Sound
        { soundSample :: SampleIndex
        , soundPeriod :: Int
        , soundVolume :: Int
        , soundTime :: Int -- How far into the sample waveform are we?
        }
    deriving (Eq, Ord, Show)

-- Silence.
noSound :: Sound
noSound = Sound 0 0 0 0

data PlayerState =
    PlayerState
        { patternIndex :: PatternIndex
        , rowIndex :: RowIndex
        , tick :: Int
        , ticksPerRow :: Int
        , tempo :: Int
        , sounds :: Vector Sound
        }
    deriving (Eq, Ord, Show)

tickSeconds :: PlayerState -> Double
tickSeconds PlayerState { tempo } =
    0.02 * fromIntegral initialTempo / fromIntegral tempo

initialPlayerState :: Int -> PlayerState
initialPlayerState numChannels = PlayerState
    { patternIndex = 0
    , rowIndex     = 0
    , tick         = 0
    , ticksPerRow  = initialTicksPerRow
    , tempo        = initialTempo
    , sounds       = V.replicate numChannels noSound
    }

-- A helper function to fetch an instruction.
instructionAt
    :: PatternIndex -> RowIndex -> ChannelIndex -> Module -> Instruction
instructionAt pi ri ci m = instructions (rows (patterns m ! pi) ! ri) ! ci

-- A helper function to set one of the sound values in the player state.
setSound :: Playback m => ChannelIndex -> Sound -> m ()
setSound ci sound = do
    modifySound ci (const sound)

-- A helper function to modify one of the sound values in the player state.
modifySound :: Playback m => ChannelIndex -> (Sound -> Sound) -> m ()
modifySound ci f = do
    modify (\ps -> ps { sounds = sounds ps V.// [(ci, f $ sounds ps V.! ci)] })

--------------------
-- Interpret module data.
--------------------

interpretEffect :: Playback m => ChannelIndex -> Effect -> m ()
interpretEffect ci effect = case effect of
    NoEffect    -> pure ()
    SetVolume v -> modifySound ci (\s -> s { soundVolume = v })
    _           -> pure ()

interpretRow :: Playback m => m ()
interpretRow = do
    PlayerState { patternIndex = pi, rowIndex = ri } <- get
    cs    <- asks channelCount
    infos <- asks sampleInfos
    forM_ [0 .. cs - 1] $ \ci -> do
        sounds <- gets sounds
        let Sound s p v t = sounds V.! ci
        Instruction ins inp effect <- asks (instructionAt pi ri ci)
        let s' = if ins == 0 then s else ins
        let p' = if inp == 0 then p else inp
        let v' = if ins == 0 then v else volume (infos ! ins)
        let t' = if inp == 0 then t else 0
        setSound ci (Sound s' p' v' t')
        interpretEffect ci effect

-- Return a function for indexing into sample s's waveform in a properly looping/cutting fashion.
waveFunction :: Playback m => SampleIndex -> m (Int -> Int)
waveFunction s = do
    wave <- asks ((! s) . sampleWaves)
    info <- asks ((! s) . sampleInfos)
    let (ro, rl) = (repeatOffset info, repeatLength info)
    let f i | rl > 2 && i < ro  = wave V.! i
            | rl > 2            = wave V.! (ro + (i - ro) `mod` rl)
            | i < V.length wave = wave V.! i
            | otherwise         = 0
    pure f

palSampleRate :: Int -> Double
palSampleRate period = palClockRate / (4.0 * fromIntegral period) -- TODO: why the 4? should be 2

playChannel :: Playback m => Double -> ChannelIndex -> m (Vector Int)
playChannel seconds ci = do
    Sound sampleIndex period volume t0 <- gets ((V.! ci) . sounds)
    let n = round (seconds * outputSampleRate)
    if (sampleIndex == 0 || period == 0)
        then pure (V.replicate n 0)
        else do
            -- Resampling. Suppose we are outputting 44100 Hz audio, and this sample must be played back at 8000Hz.
            -- Then we should "stretch out" the sample by a factor 44100/8000, which means we index its waveform
            -- at k=8000/44100 increments rounded to the nearest integer: [0,0,0,1,1,1,1,1,1,2,2,2,2,2,3,3...]
            wave <- waveFunction sampleIndex
            let k = palSampleRate period / outputSampleRate
            let t i = t0 + round (k * fromIntegral i)
            let resampled = V.generate n ((* volume) . wave . t)

            -- Advance this sound's time value.
            modifySound ci (\s -> s { soundTime = t n })
            pure resampled

--------------------
-- Advance player state.
--------------------

nextTick :: Playback m => m ()
nextTick = do
    t   <- gets tick
    tpr <- gets ticksPerRow
    let t' = (t + 1) `mod` tpr
    when (t' == 0) nextRow
    modify (\ps -> ps { tick = t' })

nextRow :: Playback m => m ()
nextRow = do
    ri <- gets rowIndex
    let ri' = (ri + 1) `mod` rowsPerPattern
    when (ri' == 0) nextPattern
    modify (\ps -> ps { rowIndex = ri' })

nextPattern :: Playback m => m ()
nextPattern = do
    pi <- gets patternIndex
    pc <- asks songPositionCount
    let pi' = (pi + 1) `mod` pc
    modify (\ps -> ps { patternIndex = pi' })

--------------------
-- Main interpret-play-mix-tell action.
--------------------

playTick :: Playback m => m ()
playTick = do
    t       <- gets tick
    seconds <- gets tickSeconds
    cs      <- asks channelCount
    when (t == 0) interpretRow
    channelAudios <- mapM (playChannel seconds) [0 .. cs - 1]
    tell [mixAndRender channelAudios]
    nextTick

-- IO monad interface to playing a module.

playModule :: Module -> Int -> IO ()
playModule m ticks = do
    let initialState       = initialPlayerState (channelCount m)
    let (finalState, pcms) = execRWS (replicateM ticks playTick) m initialState
    let pcm                = pcms `seq` B.concat pcms
    print finalState
    B.writeFile "output.pcm" pcm
