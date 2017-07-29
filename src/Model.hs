{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Model where

import           Data.Decimal
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day)
import           Control.Lens
import           Data.Monoid ((<>))

type Category = Text
type Currency = Text

data Amount = Amount !Decimal !Currency
  deriving (Eq)

instance Show Amount where
  show (Amount am cur) = show am <> " " <> T.unpack cur

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

class TransactionSource a where
  getAccounts :: a -> IO [Account]
  getTransactions :: a -> Account -> Day -> Day -> IO [Transaction]

type Rule = ReconcileResult -> ReconcileResult

type MatchedTransaction = ([Transaction], [ReportItem])

data ReconcileResult = ReconcileResult { _reconcileResultMatched :: [MatchedTransaction]
                                       , _reconcileResultNotRecorded :: [ReportItem]
                                       , _reconcileResultUnknown :: [Transaction]
                                       } deriving (Eq, Show)

makeFields ''ReconcileResult
