{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Player where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import           Types
import           Control.Monad
import           Control.Monad.RWS
import           Data.Array
import           Constants
import           Prelude                 hiding ( pi )

instructionAt
    :: PatternIndex -> RowIndex -> ChannelIndex -> Module -> Instruction
instructionAt pi ri ci m = instructions (rows (patterns m ! pi) ! ri) ! ci

type Playback m = MonadRWS Module ByteString PlayerState m

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
        , sounds :: [Sound]
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
    , sounds       = replicate numChannels noSound
    }

interpretEffect :: Playback m => ChannelIndex -> Effect -> m ()
interpretEffect ci effect = case effect of
    NoEffect -> pure ()
    _        -> pure ()

interpretRow :: Playback m => m ()
interpretRow = do
    PlayerState { patternIndex = pi, rowIndex = ri } <- get
    cs      <- asks channelCount
    infos   <- asks sampleInfos
    sounds  <- gets sounds
    sounds' <- forM (zip [0 .. cs - 1] sounds) $ \(ci, Sound s p v w) -> do
        Instruction ins inp _ <- asks (instructionAt pi ri ci)
        let s' = if ins == 0 then s else ins
        let p' = if inp == 0 then p else inp
        let v' = if ins == 0 then v else volume (infos ! ins)
        let w' = if inp == 0 then w else 0
        pure $ Sound s' p' v' w'
    modify (\s -> s { sounds = sounds' })

    forM_ [0 .. cs - 1] $ \ci -> do
        Instruction _ _ effect <- asks (instructionAt pi ri ci)
        interpretEffect ci effect

nextTick :: Playback m => m ()
nextTick = do
    t   <- gets tick
    tpr <- gets ticksPerRow
    let t' = (t + 1) `mod` tpr
    when (t' == 0) nextRow
    modify (\s -> s { tick = t' })

nextRow :: Playback m => m ()
nextRow = do
    ri <- gets rowIndex
    let ri' = (ri + 1) `mod` rowsPerPattern
    when (ri' == 0) nextPattern
    modify (\s -> s { rowIndex = ri' })

nextPattern :: Playback m => m ()
nextPattern = do
    pi <- gets patternIndex
    pc <- asks songPositionCount
    let pi' = (pi + 1) `mod` pc
    modify (\s -> s { patternIndex = pi' })

playTick :: Playback m => m ()
playTick = do
    t <- gets tick
    when (t == 0) interpretRow
    seconds <- gets tickSeconds
    -- mix the sounds and `tell` their audio
    pure ()
    nextTick


playModule :: Module -> IO ()
playModule m = do
    let initialState      = initialPlayerState (channelCount m)
    let (finalState, pcm) = execRWS (replicateM 1000 playTick) m initialState
    print finalState
    print (B.length pcm)
