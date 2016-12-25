{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Config ( readConfig
              , ExtConfig(ExtConfig)
              , baseUrl
              , token
              , Token(BasicAuthToken)
              ) where

import qualified Data.Aeson as J
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import System.Environment (lookupEnv)
import System.FilePath
import Data.Maybe

data Token = BasicAuthToken T.Text deriving (Eq, Show)

instance FromJSON Token where
  parseJSON (J.String v) = return $ BasicAuthToken v

data ExtConfig = ExtConfig { token :: Token
                           , baseUrl :: T.Text
                           }
               deriving (Show, Generic)

instance FromJSON ExtConfig

readConfig :: IO ExtConfig
readConfig = do
  home <- fromJust <$> lookupEnv "HOME"
  cf <- decodeFile (home </> "toshl.yaml") :: IO (Maybe ExtConfig)
  return $ fromJust cf
