module Accounts where

import Protolude
import Data.Map as M
import Data.Text as T
import GHC.Generics
import Data.Aeson
import Network.Wreq (asJSON, responseBody)
import Control.Lens
import Data.Maybe
import Control.Arrow (left)

import Config
import qualified Toshl

data Account = Account { balance :: Float
                       -- , currency :: Text
                       , deleted :: Bool
                       , id :: Text
                       , name :: Text
                       , status :: Text
                       } deriving (Generic, Show)

instance FromJSON Account

getAccounts :: ExtConfig -> IO (Either Text [Account])
getAccounts c = do
  r <- Toshl.get c "accounts" []
  return $ left pack . eitherDecode $ r ^. responseBody
