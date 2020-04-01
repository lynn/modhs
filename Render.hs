module Render where

import           Data.ByteString                ( ByteString )
import           Data.Vector                    ( Vector )
import           Data.List
import           Data.Word
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V

import Types

outputSampleRate :: Double
outputSampleRate = 44100.0 -- Hz

-- Input: 16-bit value.
-- Output: [low byte, high byte].
toSigned16LE :: Int -> [Word8]
toSigned16LE i
    | i > 0x7FFF    = toSigned16LE 0x7FFF
    | i < (-0x8000) = toSigned16LE (-0x8000)
    | i >= 0        = map fromIntegral [i `mod` 256, i `div` 256]
    | otherwise     = map fromIntegral [i `mod` 256, (i + 0x10000) `div` 256]

-- Input: [ch1 mono samples, ch2 mono samples, ...]
-- Output: [(l, r), (l, r), (l, r), ...] samples
mix :: [Vector Int] -> [(Int, Int)]
mix channelAudios =
    [ (mixLeft x, mixRight x) | x <- transpose $ map V.toList channelAudios ]
  where
    mixLeft  = (`div` 1024) . mixWith [3, 1, 1, 3]
    mixRight = (`div` 1024) . mixWith [1, 3, 3, 1]
    mixWith p x = sum $ zipWith (*) (cycle p) x

-- Input: [(l, r), (l, r), (l, r), ...] samples
-- Output: 16-bit signed stereo PCM bytestring.
toPCM :: [(Int, Int)] -> ByteString
toPCM = B.pack . concatMap toSigned16LE . concatMap (\(l, r) -> [l, r])

mixAndRender :: [Waveform] -> ByteString
mixAndRender = toPCM . mix