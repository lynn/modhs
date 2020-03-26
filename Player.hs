{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Player where

import           Control.Monad
import           Control.Monad.RWS
import           Data.Array
import           Data.Bits
import           Data.ByteString                ( ByteString )
import           Data.Vector                    ( Vector )
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
        , soundTime   :: Int -- How far into the sample waveform are we?
        , soundArp1   :: Int -- How many steps up to play on tick 1 mod 3?
        , soundArp2   :: Int -- How many steps up to play on tick 2 mod 3?
        }
    deriving (Eq, Ord, Show)

-- Silence.
noSound :: Sound
noSound = Sound 0 0 0 0 0 0

data Jump = Jump SongPosition RowIndex
    deriving (Eq, Ord, Show)

data PlayerState =
    PlayerState
        { songPosition  :: SongPosition
        , rowIndex      :: RowIndex
        , tick          :: Int
        , ticksPerRow   :: Int
        , tempo         :: Int
        , sounds        :: Vector Sound
        , scheduledJump :: Maybe Jump
        }
    deriving (Eq, Ord, Show)

tickSeconds :: PlayerState -> Double
tickSeconds PlayerState { tempo } =
    0.02 * fromIntegral initialTempo / fromIntegral tempo

finetuneFactor :: SampleInfo -> Double
finetuneFactor SampleInfo { finetune } =
    let semitones = fromIntegral finetune / 8 in 2 ** (semitones / 12)

initialPlayerState :: Int -> PlayerState
initialPlayerState numChannels = PlayerState
    { songPosition  = 0
    , rowIndex      = 0
    , tick          = 0
    , ticksPerRow   = initialTicksPerRow
    , tempo         = initialTempo
    , sounds        = V.replicate numChannels noSound
    , scheduledJump = Nothing
    }

-- A helper function to fetch an instruction.
instructionAt
    :: SongPosition -> RowIndex -> ChannelIndex -> Module -> Instruction
instructionAt sp ri ci m = instructions (rows (patterns m ! pi) ! ri) ! ci
    where pi = patternTable m ! sp

-- A helper function to set one of the sound values in the player state.
setSound :: Playback m => ChannelIndex -> Sound -> m ()
setSound ci sound = modifySound ci (const sound)

-- A helper function to modify one of the sound values in the player state.
modifySound :: Playback m => ChannelIndex -> (Sound -> Sound) -> m ()
modifySound ci f =
    modify (\ps -> ps { sounds = sounds ps V.// [(ci, f $ sounds ps V.! ci)] })

--------------------
-- Jumps
--------------------

executeScheduledJump :: Playback m => m ()
executeScheduledJump = do
    sj <- gets scheduledJump
    forM_ sj $ \(Jump sp ri) ->
        modify (\ps -> ps { songPosition = sp, rowIndex = ri })
    modify (\ps -> ps { scheduledJump = Nothing })

scheduleJump :: Playback m => Jump -> m ()
scheduleJump jump = modify (\ps -> ps { scheduledJump = Just jump })

-- Schedule a position jump (Bxx) to be executed on the next row.
schedulePositionJump :: Playback m => Int -> m ()
schedulePositionJump sp = do
    spc <- asks songPositionCount
    when (sp < spc) $ scheduleJump (Jump sp 0)

-- Schedule a pattern break (Dxx) to be executed on the next row.
schedulePatternBreak :: Playback m => Int -> m ()
schedulePatternBreak ri = do
    spc <- asks songPositionCount
    sp  <- nextSongPosition
    let sp' = (sp + 1) `mod` spc
    when (ri < rowsPerPattern) $ scheduleJump (Jump sp' ri)

--------------------
-- Interpret module data.
--------------------

interpretEffect :: Playback m => ChannelIndex -> Effect -> m ()
interpretEffect ci effect = case effect of
    NoEffect          -> pure ()
    Arpeggio x y -> modifySound ci (\s -> s { soundArp1 = x, soundArp2 = y })
    PositionJump   sp -> schedulePositionJump sp
    SetVolume      v  -> modifySound ci (\s -> s { soundVolume = v })
    PatternBreak   ri -> schedulePatternBreak ri
    SetTempo       t  -> modify (\ps -> ps { tempo = t })
    SetTicksPerRow t  -> modify (\ps -> ps { ticksPerRow = t })
    _                 -> pure ()

interpretRow :: Playback m => m ()
interpretRow = do
    executeScheduledJump
    PlayerState { songPosition = sp, rowIndex = ri } <- get
    cs    <- asks channelCount
    infos <- asks sampleInfos
    forM_ [0 .. cs - 1] $ \ci -> do
        sounds <- gets sounds
        let Sound s p v t _ _ = sounds V.! ci
        Instruction ins inp effect <- asks (instructionAt sp ri ci)
        let s' = if ins == 0 then s else ins
        let p' = if inp == 0 then p else inp
        let v' = if ins == 0 then v else volume (infos ! ins)
        let t' = if inp == 0 then t else 0
        setSound ci (Sound s' p' v' t' 0 0)
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

arpeggioFactor :: Playback m => ChannelIndex -> m Double
arpeggioFactor ci = do
    t <- gets tick
    Sound _ _ _ _ arp1 arp2 <- gets ((V.! ci) . sounds)
    let semitones = [0, arp1, arp2] !! (t `mod` 3)
    pure $ 2 ** (fromIntegral semitones / 12)

playChannel :: Playback m => Double -> ChannelIndex -> m (Vector Int)
playChannel seconds ci = do
    Sound sampleIndex period volume t0 _ _ <- gets ((V.! ci) . sounds)
    let n = round (seconds * outputSampleRate)
    if sampleIndex == 0 || period == 0
        then pure (V.replicate n 0)
        else do
            -- Resampling. Suppose we are outputting 44100 Hz audio, and this sample must be played back at 8000Hz.
            -- Then we should "stretch out" the sample by a factor 44100/8000, which means we index its waveform
            -- at k=8000/44100 increments rounded to the nearest integer: [0,0,0,1,1,1,1,1,1,2,2,2,2,2,3,3...]
            wave <- waveFunction sampleIndex
            ff   <- asks (finetuneFactor . (! sampleIndex) . sampleInfos)
            af   <- arpeggioFactor ci
            let freq = palSampleRate period * ff * af
            let t i = t0 + round (freq / outputSampleRate * fromIntegral i)
            let resampled = V.generate n ((* volume) . wave . t)

            -- Advance this sound's time value.
            modifySound ci (\s -> s { soundTime = t n })
            pure resampled

--------------------
-- Advance player state.
--------------------

advanceTick :: Playback m => m ()
advanceTick = do
    t   <- gets tick
    tpr <- gets ticksPerRow
    let t' = (t + 1) `mod` tpr
    when (t' == 0) advanceRow
    modify (\ps -> ps { tick = t' })

advanceRow :: Playback m => m ()
advanceRow = do
    ri <- gets rowIndex
    let ri' = (ri + 1) `mod` rowsPerPattern
    when (ri' == 0) advancePosition
    modify (\ps -> ps { rowIndex = ri' })

nextSongPosition :: Playback m => m SongPosition
nextSongPosition = do
    sp  <- gets songPosition
    spc <- asks songPositionCount
    pure $ (sp + 1) `mod` spc

advancePosition :: Playback m => m ()
advancePosition = do
    sp' <- nextSongPosition
    modify (\ps -> ps { songPosition = sp' })

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
    advanceTick

-- IO monad interface to playing a module.

playModule :: Module -> Int -> IO ()
playModule m ticks = do
    let initialState       = initialPlayerState (channelCount m)
    let (finalState, pcms) = execRWS (replicateM ticks playTick) m initialState
    let pcm                = pcms `seq` B.concat pcms
    print finalState
    B.writeFile "output.pcm" pcm
