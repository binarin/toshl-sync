{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Toshl where

import Data.Decimal
import Data.Text
-- import Prelude hiding (sum)
-- import Opaleye
-- import Data.Text
-- import Data.Decimal
-- import Data.Time.Clock (UTCTime)
-- import           Data.Profunctor.Product (p2, p3)
-- import           Data.Profunctor.Product.Default (Default)
-- import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
-- import           Data.Time.Calendar (Day)
-- import           Control.Arrow (returnA)
-- import qualified Database.PostgreSQL.Simple as PGS

-- data CachedReply = CachedReply { _cachedReplyEtag :: Text
--                                , _cachedReplyEndpoint :: Text
--                                , _cachedReplyTimestamp :: UTCTime
--                                } deriving (Show)


-- data Currency = Currency { _currencyCode :: Text
--                          , _currencyRate :: Decimal
--                          , _currencyFixed :: Bool
--                          } deriving (Show)

newtype Currency = Currency Text deriving (Show)

data AccountStatus = AccountActive
                   | AccountInactive
                   | AccountArchived
 deriving (Show)

data Account = Account { _accountId :: Int
                       , _accountName :: Text
                       , _accountBalance :: Decimal
                       , _accountInitialBalance :: Decimal
                       , _accountCurrencyCode :: Currency
                       , _accountStatus :: AccountStatus
                       } deriving (Show)
