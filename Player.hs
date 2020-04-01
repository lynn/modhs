{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Player where

import           Data.Array
import           Data.List                      ( mapAccumL )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V
import           Lens.Micro
import           Lens.Micro.TH

import           Constants
import           Types
import           Render
import           Util

--------------------
-- Definitions
--------------------

-- A sound being played on a channel.
data Sound =
    Sound
        { _soundInstrument  :: SampleIndex
        , _soundPeriod :: Int
        , _soundVolume :: Int
        , _soundTime   :: Int
        , _soundBend   :: Double  -- in semitones
        , _soundVibratoDepth :: Double  -- in semitones
        , _soundVibratoPhase :: Double  -- in radians
        }
    deriving (Eq, Ord, Show)

makeFields ''Sound

setVibratoEffect :: Int -> Int -> Sound -> Sound
setVibratoEffect x y =
    (vibratoDepth .~ fromIntegral y / 16)
        . (vibratoPhase +~ (fromIntegral x / 64) * (2 * pi))

-- A "command": a pair of functions (before, after).
-- The MOD file is "compiled" into a list of these commands.
-- To play a tick on a channel, the before command is applied, then the after command.
type Command = (Sound -> Sound, Sound -> Sound)

-- Silence.
noSound :: Sound
noSound = Sound 0 0 0 0 0.0 0.0 0.0

-- Interpret all jumps in the song and return an infinite list of rows in playback order.
interpretJumps :: Module -> [Row]
interpretJumps Module { songPositionCount, restartPosition, patternTable, patterns }
    = go (0, 0, 0, 0, 0)
  where
    go (sp, r, spL0, rL0, loop0) =
        let
            row    = (patterns ! (patternTable ! sp)) ! r
            maxSp  = songPositionCount - 1
            nextSp = if sp == maxSp then restartPosition else sp + 1

            -- By default, the position advances like this:
            r1     = (r + 1) `mod` rowsPerPattern
            sp1    = if r1 == 0 then nextSp else sp

            -- But effects can overwrite this:
            applyJumpEffect (PositionJump k) (_, _, spL, rL, loop) =
                (clamp 0 maxSp k, 0, spL, rL, loop)
            applyJumpEffect (PatternBreak k) (_, _, spL, rL, loop) =
                (nextSp, k, spL, rL, loop)
            applyJumpEffect LoopStart (sp', r', _, _, _) = (sp', r', sp, r, 0)
            applyJumpEffect (LoopEnd k) (sp', r', spL, rL, loop)
                | loop >= k = (sp', r', spL, rL, 0)
                | otherwise = (spL, rL, spL, rL, loop + 1)
            applyJumpEffect _ position = position

            -- Apply all of this row's effects to the position 5-tuple.
            effects = [ e | Instruction s p e <- elems row ]
            next    = foldr applyJumpEffect (sp1, r1, spL0, rL0, loop0) effects
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
interpretTiming rows = zipWith makeTimedRow timings rows
  where
    makeTimedRow (t, td) row = TimedRow t td row
    initialTickDuration = tempoToDuration initialTempo
    initialTiming       = (initialTicksPerRow, initialTickDuration)
    timings             = tail $ scanl setTiming initialTiming rows
    setTiming timing row =
        foldr applyTimingEffect timing $ map (^. effect) $ elems row
    applyTimingEffect (SetTicksPerRow k) (_, td) = (k, td)
    applyTimingEffect (SetTempo       k) (t, _ ) = (t, tempoToDuration k)
    applyTimingEffect _                  timing  = timing

expandInstruction :: SampleInfos -> Int -> Instruction -> [Command]
expandInstruction infos ticks (Instruction ins inp eff) =
    (f . onset, g) : fgs
  where
    n = ticks
    onset (Sound s p v t _ _ q) =
        let isPortamento = case eff of
                Portamento _ -> True
                _            -> False
            s' = if ins == 0 || isPortamento then s else ins
            p' = if inp == 0 || isPortamento then p else inp
            v' = if ins > 0 then (infos ! ins) ^. defaultVolume else v
            t' = if inp > 0 then 0 else t
        in  Sound s' p' v' t' 0.0 0.0 q
    ((f, g) : fgs) = effectCommands eff
    effectCommands (Arpeggio x y) =
        take n $ cycle [ (bend .~ fromIntegral k, id) | k <- [0, x, y] ]
    effectCommands (Slide k) = replicate n (id, period -~ k)
    effectCommands (Portamento k) =
        replicate n (period %~ addTowards inp k, id)
    effectCommands (Vibrato x y) = replicate n (setVibratoEffect x y, id)
    effectCommands (SetVolume k) = replicate n (volume .~ clamp 0 64 k, id)
    effectCommands _             = replicate n (id, id)

palSampleRate :: Int -> Double
palSampleRate periodValue = palClockRate / (4.0 * fromIntegral periodValue) -- TODO: why the 4? should be 2

finetuneToFactor :: Semitone8thDelta -> Double
finetuneToFactor ft =
    let semitones = fromIntegral ft / 8 in 2 ** (semitones / 12)

secondsToSamples :: Double -> Int
secondsToSamples seconds = round (seconds * outputSampleRate)

playSound :: SampleInfos -> SampleWaves -> Double -> Sound -> (Sound, Waveform)
playSound infos waves (secondsToSamples -> n) (Sound s p v t0 b d q)
    | s == 0 || p == 0 = (Sound s p v t0 0.0 0.0 q, V.replicate n 0)
    | otherwise = (Sound s p v (t n) 0.0 0.0 q, V.generate n ((* v) . wf . t))
  where
    wave = waves ! s
    ff   = finetuneToFactor ((infos ! s) ^. finetune)
    ro   = (infos ! s) ^. repeatOffset
    rl   = (infos ! s) ^. repeatLength
    freq = palSampleRate p * ff * 2 ** ((b + d * sin q) / 12)
    wf i | rl > 2 && i < ro  = wave V.! i
         | rl > 2            = wave V.! (ro + (i - ro) `mod` rl)
         | i < V.length wave = wave V.! i
         | otherwise         = 0
    t i = t0 + round (freq / outputSampleRate * fromIntegral i)

playTimedRow
    :: SampleInfos
    -> SampleWaves
    -> [Sound]
    -> TimedRow
    -> ([Sound], ByteString)
playTimedRow infos waves sounds (TimedRow ticks tickDuration row) =
    (sounds', mixAndRender vs)
  where
    -- Expand each instruction in this row into commands.
    expandInstruction' = expandInstruction infos ticks
    expandedChannels   = map expandInstruction' (elems row)

    -- Function to play a sound and apply its before and after commands.
    playCommand :: Sound -> Command -> (Sound, Waveform)
    playCommand sound (f, g) =
        onFst g $ playSound infos waves tickDuration $ f sound

    -- Play a series of commands on a channel and catenate the waveforms.
    playChannel sound cs = onSnd V.concat $ mapAccumL playCommand sound cs
    (sounds', vs) = unzip $ zipWith playChannel sounds expandedChannels

playTimedRows
    :: SampleInfos -> SampleWaves -> [Sound] -> [TimedRow] -> [ByteString]
playTimedRows _ _ _ [] = []
playTimedRows infos waves sounds (row : rows) =
    bs : playTimedRows infos waves sounds' rows
    where (sounds', bs) = playTimedRow infos waves sounds row

playModule :: Module -> Int -> IO ()
playModule m@Module { sampleInfos, sampleWaves, channelCount } rowCount = do
    let rowOrder  = interpretJumps m
    let timedRows = take rowCount $ interpretTiming rowOrder
    let pcm = B.concat $ playTimedRows sampleInfos
                                       sampleWaves
                                       (replicate channelCount noSound)
                                       timedRows
    B.writeFile "output.pcm" pcm
