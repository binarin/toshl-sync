{-# LANGUAGE OverloadedStrings #-}
module TinkoffCSV (readCsv) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Csv
import qualified Data.Text as T
import           Data.Time.Calendar
import qualified Data.Vector as V
import qualified Data.Text.ICU.Convert as ICU
import System.IO.Unsafe (unsafePerformIO)

import           CSVUtils
import           Model

recodeToUtf :: B.ByteString -> B.ByteString
recodeToUtf = ICU.fromUnicode utf8 . ICU.toUnicode cp1251
  where
    utf8 = unsafePerformIO $ ICU.open "utf-8" Nothing
    cp1251 = unsafePerformIO $ ICU.open "cp1251" Nothing

readCsv :: B.ByteString -> Either String [ReportItem]
readCsv b = (map untinkoff . V.toList) <$> decodeWith options HasHeader (BL.fromStrict $ recodeToUtf b)
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ';' }

newtype TinkoffDate = TinkoffDate { undate :: Day }
newtype Tinkoff = Tinkoff { untinkoff :: ReportItem }

instance FromField TinkoffDate where
  parseField s =
      case map C8.readInt $ B.split (fromIntegral $ ord '.') s of
        (Just (day, _)):(Just (month, _)):(Just (year, _)):_ ->
            case fromGregorianValid (fromIntegral year) month day of
              Just date -> pure $ TinkoffDate date

instance FromRecord Tinkoff where
  parseRecord v = do
      date <- undate <$> v .! 0
      card <- v .! 2
      sum <- v .! 6 >>= parseDecimal
      currency <- v .! 7
      category <- v .! 9
      mcc <- v .! 10
      desc <-v .! 11
      pure $ Tinkoff $ ReportItem date card (Amount sum currency) category mcc desc
