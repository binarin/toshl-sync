{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Control.Lens
import Data.Csv
import Data.Char (ord)

type Category = Text
type Currency = Text
data Amount = Income !Double !Currency | Expense !Double !Currency
  deriving (Show)
type Tag = Text

data Transaction = Transaction
  { _transactionDate :: !Day
  , _transactionCategory :: !Category
  , _transactionAmount :: !Amount
  , _transactionTags :: ![Tag]
  , _transactionDescription :: !Text
  } deriving (Show)

makeFields ''Transaction

instance FromField Day where
  parseField s =
      case map C8.readInt $ B.split (fromIntegral $ ord '/') s of
        [Just (month, _), Just (day, _), Just (year, _)] ->
            case fromGregorianValid (fromIntegral year + 2000) month day of
              Just date -> pure date
              Nothing -> fail "Doesn't look like date"

main :: IO ()
main = putStrLn "Hello, Haskell!"
