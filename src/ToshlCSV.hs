{-# LANGUAGE OverloadedStrings #-}
module ToshlCSV (readCsv) where

import Data.Word (Word8)
import Safe (readMay)
import Data.Csv
import Data.Char (ord, chr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Decimal (Decimal, DecimalRaw(..))
import qualified Data.Vector as V

import Model

readCsv :: B.ByteString -> Either String [Transaction]
readCsv b = V.toList . snd <$> decodeByName (BL.fromStrict b)

newtype AmericanDay = AmericanDay { unAmerican :: Day }

-- | Parsing american dates
--
-- >>> runParser $ unAmerican <$> parseField "5/1/17" :: Either String Day
-- Right 2017-05-01
instance FromField AmericanDay where
  parseField s =
      case map C8.readInt $ B.split (fromIntegral $ ord '/') s of
        [Just (month, _), Just (day, _), Just (year, _)] ->
            case fromGregorianValid (fromIntegral year + 2000) month day of
              Just date -> pure $ AmericanDay date
              Nothing -> fail "Not a valid date"
        invalid -> fail "Doesn't look like a date at all"

-- | Parsing numbers when ',' is used as an additional thousands delimiter
--
-- >>> runParser $ unpretty <$> parseField "15,777.35"
-- Right 15777.35
newtype PrettyDecimal = PrettyDecimal { unpretty :: Decimal } deriving (Show)

instance FromField PrettyDecimal where
  parseField s =
      case readDecimalBS (B.filter (not . chrEq ',') s) of
        Just decimal ->
          return $ PrettyDecimal decimal
        Nothing ->
          fail "Failed to parse a value as a Decimal"

chrEq :: Char -> Word8 -> Bool
chrEq a b = b == fromIntegral (ord a)

readDecimalBS :: B.ByteString -> Maybe Decimal
readDecimalBS b = readMay (C8.unpack b)

whitespaceOrComma :: Char -> Bool
whitespaceOrComma ' ' = True
whitespaceOrComma ',' = True
whitespaceOrComma _ = False

splitTags :: B.ByteString -> [Tag]
splitTags = map decodeUtf8 . filter (/= "") . B.splitWith (whitespaceOrComma . chr . fromIntegral)

instance FromNamedRecord Transaction where
    parseNamedRecord m = do
        date <- unAmerican <$> m .: "Date"
        account <- m .: "Account"
        target <- Category . decodeUtf8 <$> m .: "Category"
        income <- unpretty <$> m .: "Income amount"
        expense <- unpretty <$> m .: "Expense amount"
        currency <- m .: "Currency"
        let amount = if income > 0
                        then Amount income currency
                        else Amount (-expense) currency
        tags <- splitTags <$> m .: "Tags"
        rawDesc <- m .: "Description"
        let description = case rawDesc of
                            "" -> Nothing
                            desc -> Just desc
        pure $ Transaction date account target amount tags description
