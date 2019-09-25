{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Player where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import           Types
import           Control.Monad.RWS
import           Data.Array
import           Constants

instructionAt
    :: PatternIndex -> RowIndex -> ChannelIndex -> Module -> Instruction
instructionAt pi ri ci m = instructions (rows (patterns m ! pi) ! ri) ! ci

type Playback = RWS Module ByteString PlayerState

data Sound =
    Sound
        { soundSample :: SampleIndex
        , soundPeriod :: Int
        , soundVolume :: Int
        }
    deriving (Eq, Ord, Show)

data PlayerState =
    PlayerState
        { patternIndex :: PatternIndex
        , rowIndex :: RowIndex
        , tick :: Int
        , ticksPerRow :: Int
        , tempo :: Int
        , sounds :: [Maybe Sound]
        }
    deriving (Eq, Ord, Show)

tickSeconds :: PlayerState -> Double
tickSeconds PlayerState { tempo } =
    0.02 * fromIntegral initialTempo / fromIntegral tempo

initialPlayerState :: PlayerState
initialPlayerState =
    PlayerState 0 0 0 initialTicksPerRow initialTempo (replicate 4 Nothing)

interpretInstruction :: ChannelIndex -> Playback ()
interpretInstruction ci = do
    PlayerState { patternIndex = pi, rowIndex = ri } <- get
    i <- asks (instructionAt pi ri ci)
    -- TODO: interpret i
    pure ()

interpretRow :: Playback ()
interpretRow = do
    cs <- asks channelCount
    mapM_ interpretInstruction [0 .. cs - 1]

nextTick :: Playback ()
nextTick = do
    t   <- gets tick
    tpr <- gets ticksPerRow
    let t' = (t + 1) `mod` tpr
    when (t' == 0) nextRow
    modify (\s -> s { tick = t' })

nextRow :: Playback ()
nextRow = do
    ri <- gets rowIndex
    let ri' = (ri + 1) `mod` rowsPerPattern
    when (ri' == 0) nextPattern
    modify (\s -> s { rowIndex = ri' })

nextPattern :: Playback ()
nextPattern = do
    pi <- gets patternIndex
    pc <- asks songPositionCount
    let pi' = (pi + 1) `mod` pc
    modify (\s -> s { patternIndex = pi' })

playTick :: Playback ()
playTick = do
    t <- gets tick
    when (t == 0) interpretRow
    seconds <- gets tickSeconds
    -- mix the sounds and `tell` their audio
    pure ()
    nextTick


playModule :: Module -> IO ()
playModule m = do
    let (finalState, pcm) = execRWS (replicateM 1000 playTick) m initialPlayerState
    print finalState
    print (B.length pcm)