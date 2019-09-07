module Module where
import Data.ByteString (ByteString)

type WordCount = Integer
type PageCount = Integer -- A page is 256 words.
type SongPosition = Integer
type PatternIndex = Integer
type SampleIndex = Integer
type SemitoneDelta = Integer
type ChannelIndex = Integer
type RowIndex = Integer
type Period = Integer
type PeriodDelta = Integer

data SampleInfo =
    SampleInfo
        { name :: ByteString
        , length :: WordCount
        , finetune :: Integer
        , volume :: Integer
        , repeatOffset :: WordCount
        , repeatLength :: WordCount
        }

data SlideDirection = Up | Down | Approach

data ContinuedEffect
    = ContinuePitch
    | ContinueVibrato

data FilterState
    = FilterOff
    | FilterOn

data GlissandoMode
    = InSemitones
    | Smooth

data Waveform
    = SineWave
    | RampDown
    | SquareWave
    | RandomWave

data WaveformBehavior
    = ResetPhaseOnNewNote
    | KeepPhaseOnNewNote

data WaveformOptions
    = WaveformOptions { waveform :: Waveform, behavior :: WaveformBehavior }

data Effect
    = Arpeggio { second :: SemitoneDelta, third :: SemitoneDelta }
    | Slide { direction :: SlideDirection, delta :: PeriodDelta }
    | Vibrato { speed :: Integer, amplitude :: Integer }
    | Tremolo { speed :: Integer, amplitude :: Integer }
    | Offset { pages :: PageCount }
    | VolumeSlide { volumeDelta :: Integer, continuedEffect :: Maybe ContinuedEffect }
    | PositionJump { position :: SongPosition }
    | SetVolume { volume :: Integer }
    | PatternBreak { rowIndex :: Integer }
    | FilterControl { state :: FilterState }
    | GlissandoControl { mode :: GlissandoMode }
    | SetVibratoWaveform { waveform :: Waveform, retrigger :: Bool }
    | SetTremoloWaveform { waveform :: Waveform, retrigger :: Bool }
    | SetFinetune { finetune :: Integer }
    | LoopStart
    | LoopEnd { times :: Integer }
    | RetriggerSample { ticks :: Integer }
    | FineVolumeSlide { volumeDelta :: Integer }
    | CutSample { ticks :: Integer }
    | DelaySample { ticks :: Integer }
    | DelayPattern { rows :: Integer }
    | SetTicksPerRow { ticks :: Integer }
    | SetTempo { tempo :: Integer }

data Instruction =
    Instruction { sample :: SampleIndex, period :: Period, effect :: Maybe Effect }

data Row =
    Row { instructions :: Array ChannelIndex Instruction }

data Pattern =
    Pattern { rows :: Array RowIndex Row }

type SampleWave = ByteString

data Module =
    Module
        { title :: ByteString
        , sampleInfos :: Array SampleIndex SampleInfo
        , songPositionCount :: Integer
        , restartPosition :: SongPosition
        , patternTable :: Array SongPosition PatternIndex
        , patterns :: Array PatternIndex Pattern
        , samples :: Array SampleIndex SampleWave
        }
