module Entities.Toshl where

import ClassyPrelude

newtype AccountId = AccountId Text
newtype CurrencyCode = CurrencyCode Text
newtype AccountOrder = AccountOrder Int

data AccountStatus = StatusActive | StatusInactive | StatusArchived

data Account = Account { _accountId :: AccountId
                       , _accountParentId :: Maybe AccountId
                       , _accountName :: Text
                       , _accountCurrencyCode :: CurrencyCode
                       , _accountDeleted :: Bool
                       , _accountOrder :: AccountOrder
                       , _accountStatus :: AccountStatus
                       }
