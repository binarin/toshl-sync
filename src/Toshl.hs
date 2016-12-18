{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Toshl where
import Config

import qualified Data.ByteString as B
import qualified Network.Wreq as W
import qualified Data.Aeson as J
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Lens

type Method = T.Text
type Param = (T.Text, [T.Text])

get :: ExtConfig -> Method -> [Param] -> IO (W.Response J.Value)
get cfg@ExtConfig{..} method params = do
  let url = T.concat [baseUrl, method]
  r <- W.asJSON =<< W.getWith (W.defaults & addConfigOpts cfg & addParams params) (T.unpack url)
  return r

addConfigOpts :: ExtConfig -> W.Options -> W.Options
addConfigOpts ExtConfig{token = BasicAuthToken token} =
  W.auth ?~ W.basicAuth (encodeUtf8 token) ""

addParams :: [Param] -> W.Options -> W.Options
addParams [] opts = opts
addParams ((n, vs):ps) opts =
  opts & W.param n .~ vs & addParams ps
