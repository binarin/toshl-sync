{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
module Lib where

import           Network.Wreq
import           Control.Lens
import qualified Data.ByteString as B
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Data.Default
import qualified Data.Map as M
import           GHC.Generics
import           Data.Yaml
import           Data.Aeson



myToken = ""

credentials = auth ?~ basicAuth myToken ""

type Accounts = M.Map B.ByteString B.ByteString
type Categories = M.Map B.ByteString B.ByteString
type Tags = M.Map B.ByteString B.ByteString



readConfig :: IO ExtConfig
readConfig = undefined

fetchAccounts :: ExtConfig -> IO Accounts
fetchAccounts = undefined

someFunc :: IO ()
someFunc = return ()
