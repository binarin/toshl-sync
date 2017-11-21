module Rules.Service.Toshl
  (
    API(..)
  ) where

import ClassyPrelude
import qualified Entities.Toshl as Toshl

newtype API = API
  { getAccounts :: IO [Toshl.Account]
  }
