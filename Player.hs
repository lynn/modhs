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
import           Prelude
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V

import           Constants
import           Types
import           Render

onFst :: (a -> b) -> (a, c) -> (b, c)
onFst f (a, c) = (f a, c)

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

addTowards :: Int -> Int -> Int -> Int
addTowards goal step x
    | x > goal + step = x - step
    | x < goal - step = x + step
    | otherwise = goal

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
        , soundBend   :: Double  -- in semitones
        , soundVibratoDepth :: Double  -- in semitones
        , soundVibratoPhase :: Double  -- in radians
        }
    deriving (Eq, Ord, Show)

-- I guess I want lens...
setBend :: Double -> (Sound -> Sound)
setBend b s = s { soundBend = b }

modifyVolume :: (Int -> Int) -> (Sound -> Sound)
modifyVolume f s = s { soundVolume = f (soundVolume s) }

setVolume :: Int -> (Sound -> Sound)
setVolume v s = s { soundVolume = v }

modifyPeriod :: (Int -> Int) -> (Sound -> Sound)
modifyPeriod f s = s { soundPeriod = f (soundPeriod s) }

setVibratoDepth :: Double -> (Sound -> Sound)
setVibratoDepth d s = s { soundVibratoDepth = d }

addVibratoPhase :: Double -> (Sound -> Sound)
addVibratoPhase d s = s { soundVibratoPhase = soundVibratoPhase s + d }

setVibratoEffect :: Int -> Int -> Sound -> Sound
setVibratoEffect x y = addVibratoPhase (fromIntegral x / 64 * (2 * pi)) . setVibratoDepth (fromIntegral y / 16)

-- A "command": a pair of functions (before, after).
-- The MOD file is "compiled" into a list of these commands.
-- To play a tick on a channel, the before command is applied, then the after command.
type Command = (Sound -> Sound, Sound -> Sound)

-- Silence.
noSound :: Sound
noSound = Sound 0 0 0 0 0.0 0.0 0.0

-- Interpret all jumps in the song and return an infinite list of rows in playback order.
interpretJumps :: Module -> [Row]
interpretJumps Module { songPositionCount, restartPosition, patternTable, patterns } =
    go (0, 0, 0, 0, 0)
    where
        go (sp, r, spL, rL, loop) =
            let
                row = rows (patterns ! (patternTable ! sp)) ! r
                maxSp = songPositionCount - 1
                nextSp = if sp == maxSp then restartPosition else sp + 1

                -- By default, the position advances like this:
                r1 = (r + 1) `mod` rowsPerPattern
                sp1 = if r1 == 0 then nextSp else sp

                -- But effects can overwrite this:
                applyJumpEffect (PositionJump k) (_,   _,  spL, rL, loop) = (k', 0, spL, rL, loop) where k' = clamp 0 maxSp k
                applyJumpEffect (PatternBreak k) (_,   _,  spL, rL, loop) = (nextSp, k, spL, rL, loop)
                applyJumpEffect LoopStart        (sp', r', spL, rL, loop) = (sp', r', sp, r, 0)
                applyJumpEffect (LoopEnd k)      (sp', r', spL, rL, loop) | loop >= k = (sp', r', spL, rL, 0)
                                                                          | otherwise = (spL, rL, spL, rL, loop + 1)
                applyJumpEffect _ position = position

                -- Apply all of this row's effects to the position 5-tuple.
                effects = map effect (elems $ instructions row)
                next = foldr applyJumpEffect (sp1, r1, spL, rL, loop) effects
            in
                row : go next

tempoToDuration :: Int -> Double
tempoToDuration t = 0.02 * fromIntegral initialTempo / fromIntegral t

data TimedRow =
    TimedRow
        { ticks :: Int
        , tickDuration :: Double
        , innerRow :: Row
        }
    deriving (Eq, Show)

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

expandInstruction :: SampleInfos -> Int -> Instruction -> [Command]
expandInstruction infos ticks (Instruction ins inp effect) =
    (f.onset,g) : fgs
    where
        n = ticks
        onset (Sound s p v t b d q) =
            let
                isPortamento = case effect of { Portamento _ -> True; _ -> False }
                s' = if ins == 0 || isPortamento then s else ins
                p' = if inp == 0 || isPortamento then p else inp
                v' = if ins > 0 then volume (infos ! ins) else v
                t' = if inp > 0 then 0 else t
            in
                Sound s' p' v' t' 0.0 0.0 q
        ((f,g):fgs) = effectCommands effect
        effectCommands (Arpeggio x y) = take n $ cycle [(setBend (fromIntegral k), id) | k <- [0,x,y]]
        effectCommands (Slide k)      = replicate n (id, modifyPeriod (subtract k))
        effectCommands (Portamento k) = replicate n (modifyPeriod (addTowards inp k), id)
        effectCommands (Vibrato x y)  = replicate n (setVibratoEffect x y, id)
        effectCommands (SetVolume k)  = replicate n (setVolume (clamp 0 64 k), id)
        effectCommands _ = replicate n (id, id)

palSampleRate :: Int -> Double
palSampleRate period = palClockRate / (4.0 * fromIntegral period) -- TODO: why the 4? should be 2

finetuneFactor :: SampleInfo -> Double
finetuneFactor SampleInfo { finetune } =
    let semitones = fromIntegral finetune / 8 in 2 ** (semitones / 12)

playSound :: SampleInfos -> SampleWaves -> Double -> Sound -> (Sound, Vector Int)
playSound infos waves seconds (Sound s p v t0 b d q) =
    let
        n = round (seconds * outputSampleRate)
    in
        if s == 0 || p == 0 then
            (Sound s p v t0 0.0 0.0 q, V.replicate n 0)
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
                freq = palSampleRate p * ff * 2 ** ((b + d * sin q) / 12)
                t i = t0 + round (freq / outputSampleRate * fromIntegral i)
                resampled = V.generate n ((* v) . w . t)
            in
                (Sound s p v (t n) 0.0 0.0 q, resampled)

playTimedRow :: SampleInfos -> SampleWaves -> [Sound] -> TimedRow -> ([Sound], ByteString)
playTimedRow infos waves sounds (TimedRow ticks tickDuration row) =
    (sounds', mixAndRender vs)
    where
        expandedChannels = map (expandInstruction infos ticks) (elems $ instructions row)
        playCommand sound (f, g) = onFst g $ playSound infos waves tickDuration $ f sound
        playChannel sound cs = V.concat <$> mapAccumL playCommand sound cs
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
