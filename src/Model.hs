{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Model where

import Data.Decimal (Decimal)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day)
import Control.Lens

type Category = Text
type Currency = Text
data Amount = Amount !Decimal !Currency
  deriving (Eq, Show)
type Tag = Text
type Account = Text
data Target = Category Text | Account Account
  deriving (Eq, Show)

data Transaction = Transaction
  { _transactionDate :: !Day
  , _transactionAccount :: !Account
  , _transactionTarget :: !Target
  , _transactionAmount :: !Amount
  , _transactionTags :: ![Tag]
  , _transactionDescription :: !(Maybe Text)
  } deriving (Eq, Show)

data ReportItem = ReportItem
  { _reportItemDate :: !Day
  , _reportItemCardNo :: !(Maybe Text)
  , _reportItemAmount :: !Amount
  , _reportItemCategoryName :: !(Maybe Text)
  , _reportItemMCC :: !(Maybe Int)
  , _reportItemDescription :: !(Maybe Text)
  } deriving (Eq, Show)

makeFields ''Transaction
makeFields ''ReportItem
