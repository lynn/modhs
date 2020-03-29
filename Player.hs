{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Player where

import           Control.Monad
import           Control.Monad.RWS
import           Data.Array
import           Data.Bits
import           Data.List (mapAccumL)
import           Data.ByteString                ( ByteString )
import           Data.Vector                    ( Vector )
import           Debug.Trace
import           Prelude                 hiding ( pi )
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V

import           Constants
import           Types
import           Render

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

--------------------
-- Definitions
--------------------

data SoundData =
    SoundData
        { soundInfos :: SampleInfos
        , soundWaves :: Array SampleIndex SampleWave
        }
    deriving (Show)

-- A sound being played on a channel.
data Sound =
    Sound
        { soundIndex  :: SampleIndex
        , soundPeriod :: Int
        , soundVolume :: Int
        , soundTime   :: Int
        }
    deriving (Eq, Ord, Show)

-- Silence.
noSound :: Sound
noSound = Sound 0 0 0 0

-- Interpret all jumps in the song and return an infinite list of rows in playback order.
interpretJumps :: Module -> [Row]
interpretJumps Module { songPositionCount, restartPosition, patternTable, patterns } =
    go 0 0 0 0 0
    where
        go sp r spL rL loop =
            let
                row = rows (patterns ! (patternTable ! sp)) ! r
                effects = map effect (elems $ instructions row)
                maxSp = songPositionCount - 1
                nextSp = if sp == maxSp then restartPosition else sp + 1

                -- By default, the position advances like this:
                dr' = (r + 1) `mod` rowsPerPattern
                dsp' = if dr' == 0 then nextSp else sp

                -- But effects can overwrite this:
                applyJumpEffect (PositionJump k) (_,  _, spL, rL, loop) = (k', 0, spL, rL, loop) where k' = clamp 0 maxSp k
                applyJumpEffect (PatternBreak k) (_,  _, spL, rL, loop) = (nextSp, k, spL, rL, loop)
                applyJumpEffect LoopStart        (sp', r', spL, rL, loop) = (sp', r', sp, r, 0)
                applyJumpEffect (LoopEnd k)      (sp', r', spL, rL, loop) | loop >= k = (sp', r', spL, rL, 0)
                                                                            | otherwise = (spL, rL, spL, rL, loop + 1)
                applyJumpEffect _ position = position
                (sp', r', spL', rL', loop') = foldr applyJumpEffect (dsp', dr', spL, rL, loop) effects
            in
                row : go sp' r' spL' rL' loop'

data TimedRow =
    TimedRow
        { ticks :: Int
        , tickDuration :: Double
        , innerRow :: Row
        }
    deriving (Eq, Show)

tempoToDuration :: Int -> Double
tempoToDuration t = 0.02 * fromIntegral initialTempo / fromIntegral t

interpretTiming :: [Row] -> [TimedRow]
interpretTiming rows =
    zipWith makeTimedRow timings rows
    where
        makeTimedRow (t, td) row = TimedRow t td row
        initialTickDuration = tempoToDuration initialTempo
        initialTiming = (initialTicksPerRow, initialTickDuration)
        timings = tail $ scanl setTiming initialTiming rows
        setTiming timing row = foldr applyTimingEffect timing $ map effect $ elems $ instructions row
        applyTimingEffect (SetTicksPerRow k) (_, td) = (k, td)
        applyTimingEffect (SetTempo k) (t, _) = (t, tempoToDuration k)
        applyTimingEffect _ timing = timing

expandInstruction :: SampleInfos -> Int -> Instruction -> [Sound -> Sound]
expandInstruction infos ticks (Instruction ins inp e) =
    first : rest
    where
        loadSample (Sound s p v t) = Sound ins p v' t where v' = volume (infos!ins)
        playNote (Sound s p v t) = Sound s inp v 0
        first = (if ins == 0 then id else loadSample) . (if inp == 0 then id else playNote)
        rest = replicate (ticks - 1) id

palSampleRate :: Int -> Double
palSampleRate period = palClockRate / (4.0 * fromIntegral period) -- TODO: why the 4? should be 2

finetuneFactor :: SampleInfo -> Double
finetuneFactor SampleInfo { finetune } =
    let semitones = fromIntegral finetune / 8 in 2 ** (semitones / 12)

playSound :: SampleInfos -> SampleWaves -> Double -> Sound -> (Sound, Vector Int)
playSound infos waves seconds (Sound s p v t0) =
    let
        n = round (seconds * outputSampleRate)
    in
        if s == 0 || p == 0 then
            (Sound s p v t0, V.replicate n 0)
        else
            let
                wave = waves ! s
                ff = finetuneFactor (infos ! s)
                ro = repeatOffset (infos ! s)
                rl = repeatLength (infos ! s)
                w i | rl > 2 && i < ro  = wave V.! i
                    | rl > 2            = wave V.! (ro + (i - ro) `mod` rl)
                    | i < V.length wave = wave V.! i
                    | otherwise         = 0
                freq = palSampleRate p * ff
                t i = t0 + round (freq / outputSampleRate * fromIntegral i)
                resampled = V.generate n ((* v) . w . t)
            in
                (Sound s p v (t n), resampled)

playTimedRow :: SampleInfos -> SampleWaves -> [Sound] -> TimedRow -> ([Sound], ByteString)
playTimedRow infos waves sounds (TimedRow ticks tickDuration row) =
    (sounds', mixAndRender vs)
    where
        expandedChannels = map (expandInstruction infos ticks) (elems $ instructions row)
        playChannel sound fs = V.concat <$> mapAccumL (\s f -> playSound infos waves tickDuration (f s)) sound fs
        (sounds', vs) = unzip $ zipWith playChannel sounds expandedChannels

playTimedRows :: SampleInfos -> SampleWaves -> [Sound] -> [TimedRow] -> [ByteString]
playTimedRows infos waves sounds [] = []
playTimedRows infos waves sounds (row:rows) =
    bs : playTimedRows infos waves sounds' rows
    where (sounds', bs) = playTimedRow infos waves sounds row

playModule :: Module -> Int -> IO ()
playModule m@Module { sampleInfos, sampleWaves, channelCount } rowCount = do
    let rowOrder = interpretJumps m
    let timedRows = take rowCount $ interpretTiming rowOrder
    let initialSounds = replicate channelCount noSound
    let pcm = B.concat $ playTimedRows sampleInfos sampleWaves initialSounds timedRows
    B.writeFile "output.pcm" pcm
