{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where
import           Data.Array                     ( Array )
import           Data.Vector                    ( Vector )
import           Data.ByteString                ( ByteString )
import           GHC.Generics                   ( Generic )
import           Control.DeepSeq
import           Lens.Micro.TH

type ChannelIndex = Int
type FractionalSemitones = Double
type IndexIntoWaveform = Int
type PageCount = Int
type PatternIndex = Int
type Period = Int
type PeriodDelta = Int
type Radians = Double
type RowCount = Int
type RowIndex = Int
type InstrumentIndex = Int
type Seconds = Double
type Semitone16ths = Int -- Unit of vibrato amplitude.
type Semitone8thDelta = Int -- Unit of finetune.
type SemitoneDelta = Int -- Unit of arpeggio.
type SongPosition = Int
type TempoValue = Int
type TickCount = Int
type Volume = Int
type VolumeDelta = Int -- Unit of tremolo amplitude and volume slides.
type WaveEffectSpeed = Int -- Advance phase by this many 16ths of a period per row.
type WordCount = Int

data Instrument =
    Instrument
        { _instrumentName          :: ByteString
        , _instrumentLength        :: WordCount
        , _instrumentFinetune      :: Semitone8thDelta
        , _instrumentDefaultVolume :: Volume
        , _instrumentRepeatOffset  :: WordCount
        , _instrumentRepeatLength  :: WordCount
        }
    deriving (Eq, Ord, Show, Generic, NFData)

makeFields ''Instrument

type Waveform = Vector Int

type Instruments = Array InstrumentIndex Instrument
type Waveforms = Array InstrumentIndex Waveform

data ContinuedEffect
    = ContinueSlide
    | ContinueVibrato
    deriving (Eq, Ord, Show, Generic, NFData)

data FilterState
    = FilterOff
    | FilterOn
    deriving (Eq, Ord, Show, Generic, NFData)

data GlissandoMode
    = InSemitones
    | Smooth
    deriving (Eq, Ord, Show, Generic, NFData)

data WaveformShape
    = SineWave
    | RampDown
    | SquareWave
    | RandomWave
    deriving (Eq, Ord, Show, Generic, NFData)

data WaveformBehavior
    = ResetPhaseOnNewNote
    | KeepPhaseOnNewNote
    deriving (Eq, Ord, Show, Generic, NFData)

data WaveformOptions
    = WaveformOptions WaveformShape WaveformBehavior
    deriving (Eq, Ord, Show, Generic, NFData)

data Effect
    = NoEffect
    | {- 0xx     -} Arpeggio SemitoneDelta SemitoneDelta
    | {- 1xx 2xx -} Slide PeriodDelta
    | {- 3xx     -} Portamento PeriodDelta
    | {- 4xx     -} Vibrato WaveEffectSpeed Semitone16ths
    | {- 7xx     -} Tremolo WaveEffectSpeed VolumeDelta
    | {- 9xx     -} Offset PageCount
    | {- 5,6,Axx -} VolumeSlide VolumeDelta (Maybe ContinuedEffect)
    | {- Bxx     -} PositionJump SongPosition
    | {- Cxx     -} SetVolume Volume
    | {- Dxx     -} PatternBreak RowIndex
    | {- E0x     -} FilterControl FilterState
    | {- E3x     -} GlissandoControl GlissandoMode
    | {- E4x     -} SetVibratoWaveform WaveformOptions
    | {- E7x     -} SetTremoloWaveform WaveformOptions
    | {- E5x     -} SetFinetune Semitone8thDelta
    | {- E60     -} LoopStart
    | {- E6x     -} LoopEnd Int
    | {- E9x     -} RetriggerSample TickCount
    | {- EAx EBx -} FineVolumeSlide VolumeDelta
    | {- ECx     -} CutSample TickCount
    | {- EDx     -} DelaySample TickCount
    | {- EEx     -} DelayPattern RowCount
    | {- Fxx <32 -} SetTicksPerRow TickCount
    | {- Fxx ≥32 -} SetTempo TempoValue
    | UnknownEffect { effectNumber :: Int, argument :: Int }
    deriving (Eq, Ord, Show, Generic, NFData)

data Instruction =
    Instruction
        { _instructionInstrumnet :: InstrumentIndex
        , _instructionPeriod :: Period
        , _instructionEffect :: Effect
        }
    deriving (Eq, Ord, Show, Generic, NFData)

makeFields ''Instruction

type Row = Array ChannelIndex Instruction
type Pattern = Array RowIndex Row

data Module =
    Module
        { _moduleTitle             :: ByteString
        , _moduleInstruments       :: Instruments
        , _moduleSongPositionCount :: Int
        , _moduleRestartPosition   :: SongPosition
        , _modulePatternTable      :: Array SongPosition PatternIndex
        , _modulePatterns          :: Array PatternIndex Pattern
        , _moduleWaveforms         :: Waveforms
        , _moduleChannelCount      :: Int
        }
    deriving (Eq, Ord, Show, Generic, NFData)

makeFields ''Module
