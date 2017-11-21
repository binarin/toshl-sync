module Adapters.Service.Toshl.API
  ( Config(..)
  , withHandle
  ) where

import ClassyPrelude

import qualified Rules.Service.Toshl as ToshlService
import qualified Entities.Toshl as Toshl

data Config = Config { _configPersonalToken :: Text
                     }

withHandle :: Config -> (ToshlService.API -> IO a) -> IO a
withHandle cfg action = do
  let handle = ToshlService.API (getAccounts cfg)
  action handle

getAccounts :: Config -> IO [Toshl.Account]
getAccounts = undefined
