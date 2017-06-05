module CSVUtils (parseDecimal) where

import           Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.Csv
import           Data.Decimal
import           Data.Word
import           Safe (readMay)

parseDecimal :: B.ByteString -> Parser Decimal
parseDecimal s =
    case readMay (C8.unpack $ B.map commaToPeriod s) of
      Just dec -> pure dec
      Nothing -> fail $ "Failed to read decimal " ++ C8.unpack s
  where
    commaToPeriod :: Word8 -> Word8
    commaToPeriod c
        | c == (fromIntegral $ ord ',') = fromIntegral $ ord '.'
        | otherwise = c
