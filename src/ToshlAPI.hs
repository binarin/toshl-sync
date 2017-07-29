{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module ToshlAPI where

import           Control.Lens
import           Control.Exception.Safe
import           Control.Lens
import qualified Data.Text as T
import           Data.Aeson
import           Data.Text (Text)
import qualified Network.Wreq as W
import           Data.Text.Encoding (encodeUtf8)

import           Config

newtype Currency = Currency Text deriving (Show)

data AccountStatus = AccountActive deriving (Show)

data Account = Account { _accountId :: Text
                       , _accountName :: Text
                       , _accountBalance :: Double
                       , _accountCurrency :: Currency
                       , _accountStatus :: AccountStatus
                       , _accountOrder :: Int
                       } deriving (Show)
makeFields ''Account

data ErrorContext = ErrorContext { verb :: Text
                                 , method :: Method
                                 , params :: [Param]
                                 } deriving (Show)

instance FromJSON AccountStatus where
  parseJSON = withText "Account status" $ \"active" -> pure AccountActive

instance FromJSON Currency where
  parseJSON = withObject "Currency" $ \v -> Currency <$> v .: "code"

instance FromJSON Account where
  parseJSON = withObject "Account" $ \v -> Account <$> v .: "id"
                                                   <*> v .: "name"
                                                   <*> v .: "balance"
                                                   <*> v .: "currency"
                                                   <*> v .: "status"
                                                   <*> v .: "order"

type Method = Text
type Param = (Text, [Text])

get :: FromJSON a => Config -> Method -> [Param] -> IO a
get cfg@Config{toshlUrl} method params = do
    let url = T.concat [toshlUrl, method]
        opts = W.defaults & addConfigOpts cfg
                          & addParams params
    resp <- W.asJSON =<< W.getWith opts (T.unpack url)
    pure $ unwrap resp

unwrap :: W.Response a -> a
unwrap resp = resp ^. W.responseBody

addConfigOpts :: Config -> W.Options -> W.Options
addConfigOpts Config{toshlKey} =
  W.auth ?~ W.basicAuth (encodeUtf8 toshlKey) ""

addParams :: [Param] -> W.Options -> W.Options
addParams [] opts = opts
addParams ((n, vs):ps) opts =
  opts & W.param n .~ vs & addParams ps
