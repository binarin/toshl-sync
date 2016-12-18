{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Accounts where

import Data.Map as M
import Data.Text as T
import GHC.Generics
import Data.Aeson

import Config
import Toshl

data Account = Account { balance :: Float
                       , currency :: Text
                       , deleted :: Bool
                       , id :: Text
                       , name :: Text
                       , status :: Text
                       } deriving (Generic, Show)

instance FromJSON Account

getAccounts :: ExtConfig -> IO [Account]
getAccounts = undefined
