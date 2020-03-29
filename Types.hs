{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types where
import           Data.Array                     ( Array )
import           Data.Vector                    ( Vector )
import           Data.ByteString                ( ByteString )
import           GHC.Generics                   ( Generic )
import           Control.DeepSeq

type WordCount     = Int
type PageCount     = Int
type SongPosition  = Int
type PatternIndex  = Int
type SampleIndex   = Int
type SemitoneDelta = Int
type ChannelIndex  = Int
type RowIndex      = Int
type Period        = Int
type PeriodDelta   = Int

data SampleInfo =
    SampleInfo
        { name         :: ByteString
        , length       :: WordCount
        , finetune     :: Int
        , volume       :: Int
        , repeatOffset :: WordCount
        , repeatLength :: WordCount
        }
    deriving (Eq, Ord, Show, Generic, NFData)

type SampleWave = Vector Int

type SampleInfos = Array SampleIndex SampleInfo
type SampleWaves = Array SampleIndex SampleWave

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

data Waveform
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
    = WaveformOptions { waveform :: Waveform, behavior :: WaveformBehavior }
    deriving (Eq, Ord, Show, Generic, NFData)

data Effect
    = NoEffect
    | Arpeggio { second :: SemitoneDelta, third :: SemitoneDelta }
    | Slide { delta :: PeriodDelta }
    | Portamento { speed :: PeriodDelta }
    | Vibrato { speed :: Int, amplitude :: Int }
    | Tremolo { speed :: Int, amplitude :: Int }
    | Offset { pages :: PageCount }
    | VolumeSlide { volumeDelta :: Int, continuedEffect :: Maybe ContinuedEffect }
    | PositionJump { position :: SongPosition }
    | SetVolume { newVolume :: Int }
    | PatternBreak { newRowIndex :: Int }
    | FilterControl { state :: FilterState }
    | GlissandoControl { mode :: GlissandoMode }
    | SetVibratoWaveform { options :: WaveformOptions }
    | SetTremoloWaveform { options :: WaveformOptions }
    | SetFinetune { newFinetune :: Int }
    | LoopStart
    | LoopEnd { times :: Int }
    | RetriggerSample { ticks :: Int }
    | FineVolumeSlide { volumeDelta :: Int }
    | CutSample { ticks :: Int }
    | DelaySample { ticks :: Int }
    | DelayPattern { delayRows :: Int }
    | SetTicksPerRow { newTicksPerRow :: Int }
    | SetTempo { newTempo :: Int }
    | UnknownEffect { effectNumber :: Int, argument :: Int }
    deriving (Eq, Ord, Show, Generic, NFData)

data Instruction =
    Instruction { sample :: SampleIndex, period :: Period, effect :: Effect }
    deriving (Eq, Ord, Show, Generic, NFData)

newtype Row =
    Row { instructions :: Array ChannelIndex Instruction }
    deriving (Eq, Ord, Show, Generic, NFData)

newtype Pattern =
    Pattern { rows :: Array RowIndex Row }
    deriving (Eq, Ord, Show, Generic, NFData)

data Module =
    Module
        { title             :: ByteString
        , sampleInfos       :: SampleInfos
        , songPositionCount :: Int
        , restartPosition   :: SongPosition
        , patternTable      :: Array SongPosition PatternIndex
        , patterns          :: Array PatternIndex Pattern
        , sampleWaves       :: SampleWaves
        , channelCount      :: Int
        }
    deriving (Eq, Ord, Show, Generic, NFData)
