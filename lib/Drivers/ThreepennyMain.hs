module ThreepennyMain(main) where

import ClassyPrelude

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

data Env = Env {
               }

mkEnv :: IO Env
mkEnv = return Env

main :: IO ()
main = do
  env <- mkEnv
  startGUI mkTPConfig (setup env)

mkTPConfig = defaultConfig

setup :: Env -> Window -> UI ()
setup env _ = undefined
