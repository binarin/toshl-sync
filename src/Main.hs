{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Vector (Vector)
import Text.RawString.QQ
import Data.Maybe
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Control.Lens
import Data.Csv
import Data.Char (ord, chr)
import qualified Data.ByteString.Lazy as BL

type Category = Text
type Currency = Text
data Amount = Amount !Double !Currency
  deriving (Show)
type Tag = Text
type Account = Text
data Target = Category Text | Account Account
  deriving (Show)

newtype PrettyDouble = PrettyDouble { unpretty :: Double } deriving (Show)

data Transaction = Transaction
  { _transactionDate :: !Day
  , _transactionAccount :: !Account
  , _transactionTarget :: !Target
  , _transactionAmount :: !Amount
  , _transactionTags :: ![Tag]
  , _transactionDescription :: !(Maybe Text)
  } deriving (Show)

makeFields ''Transaction

-- >>> runParser $ parseField "5/1/17" :: Either String Day
-- Right 2017-05-01
instance FromField Day where
  parseField s =
      case map C8.readInt $ B.split (fromIntegral $ ord '/') s of
        [Just (month, _), Just (day, _), Just (year, _)] ->
            case fromGregorianValid (fromIntegral year + 2000) month day of
              Just date -> pure date
              Nothing -> fail "Not a valid date"
        invalid -> fail "Doesn't look like a date at all"

instance FromField PrettyDouble where
  parseField s = PrettyDouble <$> parseField (B.filter (/= (fromIntegral $ ord ',')) s)

whitespaceOrComma :: Char -> Bool
whitespaceOrComma ' ' = True
whitespaceOrComma ',' = True
whitespaceOrComma _ = False

splitTags :: B.ByteString -> [Tag]
splitTags = map decodeUtf8 . filter (/= "") . B.splitWith (whitespaceOrComma . chr . fromIntegral)

instance FromNamedRecord Transaction where
    parseNamedRecord m = do
        date <- m .: "Date"
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

csvSample :: BL.ByteString
csvSample = BL.fromStrict $ encodeUtf8 [r|"Date","Account","Category","Tags","Expense amount","Income amount","Currency","In main currency","Main currency","Description"
5/1/17,tcs-credit,Связь,ucanet,555.00,0,RUB,555.00,RUB,
5/15/17,binarin-alfa,Salary,Mirantis,0,"26,000.00",RUB,"26,000.00",RUB,
|]

main :: IO ()
main = putStrLn "Hello, Haskell!"
