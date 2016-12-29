module Toshl where

import Protolude
import Config

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq as W
import qualified Data.Aeson as J
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Lens hiding ((&))

type Method = T.Text
type Param = (T.Text, [T.Text])

get :: ExtConfig -> Method -> [Param] -> IO (W.Response BL.ByteString)
get cfg@ExtConfig{..} method params = do
  let url = T.concat [baseUrl, method]
  W.getWith (W.defaults & addConfigOpts cfg & addParams params) (T.unpack url)

addConfigOpts :: ExtConfig -> W.Options -> W.Options
addConfigOpts ExtConfig{token = BasicAuthToken token} =
  W.auth ?~ W.basicAuth (encodeUtf8 token) ""

addParams :: [Param] -> W.Options -> W.Options
addParams [] opts = opts
addParams ((n, vs):ps) opts =
  opts & W.param n .~ vs & addParams ps
