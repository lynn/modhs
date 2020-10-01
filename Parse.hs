{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parse where

-- http://lclevy.free.fr/mo3/mod.txt

import           Prelude                 hiding ( length )
import qualified Data.Attoparsec.ByteString    as P
import           Data.Attoparsec.ByteString     ( Parser )
import qualified ParseBinary                   as P
import           Data.Array.IArray
import qualified Data.Vector                   as V
import           Control.Applicative
import           Control.Monad
import           Types
import           Data.Bits
import           Lens.Micro

pSignature :: Parser ()
pSignature = void $ P.choice $ map P.string ["M.K.", "4CHN", "M!K!", "FLT4"]

pInstrument :: Parser Instrument
pInstrument =
    Instrument
        <$> P.take 22 -- name
        <*> P.u16 -- length
        <*> P.u8 -- finetune
        <*> P.u8 -- volume
        <*> P.u16 -- repeatOffset
        <*> P.u16 -- repeatLength

pArray :: (Ix i, Num i) => i -> Int -> Parser a -> Parser (Array i a)
pArray start count p =
    listArray (start, start + fromIntegral count - 1) <$> P.count count p

decodeEffect :: Int -> Int -> Effect
-- TODO 9EF
decodeEffect 0x0 x                 = Arpeggio (x `div` 16) (x `mod` 16)
decodeEffect 0x1 x                 = Slide x
decodeEffect 0x2 x                 = Slide (-x)
decodeEffect 0x3 x                 = Portamento x
decodeEffect 0x4 x                 = Vibrato (x `div` 16) (x `mod` 16)
decodeEffect 0x5 x = VolumeSlide (volumeSlideDelta x) (Just ContinueSlide)
decodeEffect 0x6 x = VolumeSlide (volumeSlideDelta x) (Just ContinueVibrato)
decodeEffect 0x7 x                 = Tremolo (x `div` 16) (x `mod` 16)
decodeEffect 0xA x                 = VolumeSlide (volumeSlideDelta x) Nothing
decodeEffect 0xB x                 = PositionJump x
decodeEffect 0xC x                 = SetVolume x
decodeEffect 0xD x                 = PatternBreak x
decodeEffect 0xF x | x > 0, x < 32 = SetTicksPerRow x
decodeEffect 0xF x | x >= 32       = SetTempo x
decodeEffect e x                   = UnknownEffect e x

volumeSlideDelta :: Int -> Int
volumeSlideDelta x = if x >= 0x10 then x `div` 0x10 else -x

pInstruction :: Parser Instruction
pInstruction = do
    -- Byte  1   Byte  2   Byte  3   Byte 4
    -- --------- --------- --------- ---------
    -- 7654-3210 7654-3210 7654-3210 7654-3210
    -- wwww XXXX xxxxxxxxx yyyy eeee aaaaaaaaa
    --
    --     wwwwyyyy ( 8 bits) : sample number
    -- XXXXxxxxxxxx (12 bits) : sample 'period'
    -- eeeeaaaaaaaa (12 bits) : effect and argument
    --
    wx <- P.u16
    ye <- P.u8
    a  <- P.u8
    let w  = shiftR wx 12
    let y  = shiftR ye 4
    let e  = ye .&. 0x0F
    let ii = shiftL w 4 .|. y
    let ip = wx .&. 0x0FFF
    let ie = decodeEffect e a
    pure $ Instruction ii ip ie

pRow :: Parser Row
pRow = pArray 0 4 pInstruction

pPattern :: Parser Pattern
pPattern = pArray 0 64 pRow

pModule :: Parser Module
pModule = do
    mTitle             <- P.take 20
    mInstruments       <- pArray 1 31 pInstrument <|> pArray 1 15 pInstrument
    mSongPositionCount <- P.u8
    mRestartPosition   <- P.u8
    mPatternTable      <- pArray 0 128 P.u8
    pSignature
    let patternCount = maximum mPatternTable + 1
    mPatterns  <- pArray 0 patternCount pPattern
    mWaveforms <- forM mInstruments $ \i -> V.replicateM (i ^. length) P.i16
    let mChannelCount = 4
    pure $ Module mTitle
                  mInstruments
                  mSongPositionCount
                  mRestartPosition
                  mPatternTable
                  mPatterns
                  mWaveforms
                  mChannelCount
