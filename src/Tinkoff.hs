module Tinkoff
  (
    parseCsv,
    TransactionStatus,
    Currency,
    Amount,
    Category,
    CardTransaction(..),
  ) where

import Protolude

import qualified Codec.Text.IConv as IConv
import           Control.Arrow (left)
import           Control.Monad (fail)
import qualified Data.ByteString.Char8 as C8
import           Data.Csv ((.!))
import qualified Data.Csv as CSV
import           Data.Decimal
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.LocalTime
import           Data.Time.Parse (strptime)
import           Data.Vector (Vector)
import qualified Data.Vector as V

data TransactionStatus = OK
  deriving (Eq, Ord, Show)

type Currency = Text
data Amount = Amount Decimal Currency
  deriving (Eq, Ord, Show)

data Category = Category Text (Maybe Int)
  deriving (Eq, Ord, Show)

data CardTransaction =
  CardTransaction { holdDate :: LocalTime
                  , settlementDate :: Maybe LocalTime
                  , cardId :: Text
                  , status :: TransactionStatus
                  , trnAmount :: Amount
                  , cardAmount :: Amount
                  , category :: Category
                  , comment :: Text
                  }
  deriving (Eq, Show, Ord)

parseCsv :: LByteString -> Either Text (Vector CardTransaction)
parseCsv = errorShouldBeText . CSV.decode CSV.NoHeader . fixCharset
  where
    errorShouldBeText = left T.pack
    fixCharset = IConv.convert "CP1251" "UTF-8"

newtype Timestamp = Timestamp { getLocalTime :: LocalTime }

instance CSV.FromField Timestamp where
  parseField s = case parseDate s of
    Just lt -> return $ Timestamp lt
    Nothing -> fail "Invalid timestamp"

instance CSV.FromField TransactionStatus where
  parseField "OK" = return OK
  parseField _ = fail "Unknown transaction status"

instance CSV.FromRecord CardTransaction where
  parseRecord v
    | V.length v == 12 = do
        holdDate <- getLocalTime <$> v .! 0
        settlementDate <- parseDate <$> v .! 1
        cardId <- v .! 2
        status <- v .! 3
        trnAmount <- Amount <$> (fmap getDecimal $ v .! 4) <*> v .! 5
        cardAmount <- Amount <$> (fmap getDecimal $ v .! 6) <*> v .! 7
        category <- Category <$> v .! 8 <*> v .! 9
        comment <- v .! 10
        return CardTransaction{..}

newtype DecimalField = DecimalField { getDecimal :: Decimal }

instance CSV.FromField DecimalField where
  parseField s = case readMaybe (map replaceCommaWithDot . C8.unpack $ s) of
    Just decimal -> return $ DecimalField decimal
    Nothing -> fail $ "failed to parse as a decimal number: '" ++ C8.unpack s ++ "'"
    where
      replaceCommaWithDot ',' = '.'
      replaceCommaWithDot c = c

parseDate :: ByteString -> Maybe LocalTime
parseDate = fmap fst . strptime "%d.%m.%Y %H:%M:%S"
