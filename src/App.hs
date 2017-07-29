{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import Graphics.UI.Threepenny.Core
import Control.Monad.State
import Control.Monad.Reader
import Data.Decimal
import Data.Text

data Account' = Account' { _accountId :: Int
                         , _accountName :: Text
                         , _accountBalance :: Decimal
                         }

class Application a where
  getAccounts :: a [Account']

data AppConfig = AppConfig { _appToshlKey :: Text
                           , _appToshlUrl :: Text
                           }
data AppState = AppState {}

newtype App a = App { runA :: ReaderT AppConfig (StateT AppState UI) a }
 deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

instance MonadUI App where
  liftUI :: UI a -> App a
  liftUI = App . lift . lift

runApp :: App a -> AppConfig -> AppState -> UI (a, AppState)
runApp app cfg state = runStateT (runReaderT (runA app) cfg) state

instance Application App where
  getAccounts = pure [ Account' 1 "binarin-tcs" 100
                     , Account' 2 "tcs-credit" 500
                     , Account' 3 "binarin-wallet" (-700)]
