module ParseBinary
    ( u8
    , i8
    , u16
    , i16
    )
where

import qualified Data.Attoparsec.ByteString    as P
import           Data.Attoparsec.ByteString     ( Parser )
import           Data.Bits
import           Data.Int
import           Data.Word

u8 :: Parser Int
u8 = fromIntegral <$> P.anyWord8

i8 :: Parser Int
i8 = do
    u <- P.anyWord8
    let i = fromIntegral u :: Int8
    pure (fromIntegral i)

u16 :: Parser Int
u16 = do
    u <- P.anyWord8
    v <- P.anyWord8
    let iu = fromIntegral u :: Word16
    let iv = fromIntegral v :: Word16
    pure (fromIntegral (shiftL iu 8 .|. iv))

i16 :: Parser Int
i16 = do
    u <- P.anyWord8
    v <- P.anyWord8
    let iu = fromIntegral u :: Int16
    let iv = fromIntegral v :: Int16
    pure (fromIntegral (shiftL iu 8 .|. iv))

