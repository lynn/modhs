{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Player where

import           Constants
import           Control.Monad
import           Control.Monad.RWS
import           Data.Array
import           Data.ByteString                ( ByteString )
import           Data.Vector                    ( Vector )
import           Data.Word
import           Debug.Trace
import           Prelude                 hiding ( pi )
import           Types
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V

instructionAt
    :: PatternIndex -> RowIndex -> ChannelIndex -> Module -> Instruction
instructionAt pi ri ci m = instructions (rows (patterns m ! pi) ! ri) ! ci

type Playback m = MonadRWS Module [ByteString] PlayerState m

data Sound =
    Sound
        { soundSample :: SampleIndex
        , soundPeriod :: Int
        , soundVolume :: Int
        , soundWord :: Int
        }
    deriving (Eq, Ord, Show)

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

interpretEffect :: Playback m => ChannelIndex -> Effect -> m ()
interpretEffect ci effect = case effect of
    NoEffect -> pure ()
    _        -> pure ()

interpretRow :: Playback m => m ()
interpretRow = do
    PlayerState { patternIndex = pi, rowIndex = ri } <- get
    cs    <- asks channelCount
    infos <- asks sampleInfos
    forM_ [0 .. cs - 1] $ \ci -> do
        sounds <- gets sounds
        let Sound s p v w = sounds V.! ci
        Instruction ins inp effect <- asks (instructionAt pi ri ci)
        let s' = if ins == 0 then s else ins
        let p' = if inp == 0 then p else inp
        let v' = if ins == 0 then v else volume (infos ! ins)
        let w' = if inp == 0 then w else 0
        modify (\ps -> ps { sounds = sounds V.// [(ci, Sound s' p' v' w')] })
        interpretEffect ci effect

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

playSample :: Playback m => Double -> ChannelIndex -> m (Vector Int)
playSample seconds ci = do
    sounds      <- gets sounds
    sampleInfos <- asks sampleInfos
    sampleWaves <- asks sampleWaves
    let n = round (seconds * outputBitRate)
    case sounds V.! ci of
        Sound 0 _ _ _ -> pure (V.replicate n 0)
        Sound _ 0 _ _ -> pure (V.replicate n 0)
        Sound s p v w -> do
            let wave = sampleWaves ! s
            let info = sampleInfos ! s
            let ro   = repeatOffset info
            let rl   = repeatLength info
            let sampleAt i | rl > 2 && i < ro  = wave V.! i
                           | rl > 2 = wave V.! (ro + (i - ro) `mod` rl)
                           | i < V.length wave = wave V.! i
                           | otherwise         = 0
            let rate = palClockRate / (4.0 * fromIntegral p) -- why the 4? should be 2
            let k    = rate / outputBitRate
            let sampleIndexAt i = w + round (k * fromIntegral i)
            let resampled = V.generate n (sampleAt . sampleIndexAt)
            let w'        = sampleIndexAt n
            -- Update this sound's w.
            modify (\ps -> ps { sounds = sounds V.// [(ci, Sound s p v w')] })
            pure resampled

toSigned16 :: Int -> [Word8]
toSigned16 i
    | i > 0x7FFF    = toSigned16 0x7FFF
    | i < (-0x8000) = toSigned16 (-0x8000)
    | i >= 0        = map fromIntegral [i `div` 256, i `mod` 256]
    | otherwise     = map fromIntegral [(i + 0x10000) `div` 256, i `mod` 256]

playTick :: Playback m => m ()
playTick = do
    t <- gets tick
    when (t == 0) interpretRow
    seconds <- gets tickSeconds
    cs      <- asks channelCount
    audios  <- mapM (playSample seconds) [0 .. cs - 1]
    let mixed = fmap (`div` 4) $ foldl1 (V.zipWith (+)) audios
    let bytes = B.pack (concatMap (reverse . toSigned16) mixed)
    tell [bytes]
    nextTick


playModule :: Module -> Int -> IO ()
playModule m ticks = do
    let initialState       = initialPlayerState (channelCount m)
    let (finalState, pcms) = execRWS (replicateM ticks playTick) m initialState
    let pcm                = pcms `seq` B.concat pcms
    -- let pcm = B.pack . concatMap toSigned16 . V.toList . (!1) . sampleWaves $ m
    B.writeFile "output.pcm" pcm
