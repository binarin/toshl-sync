module Main where

import Control.Exception (bracket)
import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import Test.DocTest (doctest)
import System.Environment

main :: IO ()
main = do
    traverse_ putStrLn args -- optionally print arguments
    nixLibDir <- lookupEnv "NIX_GHC_LIBDIR"
    case nixLibDir of
      Just ld -> bracket (setEnv "GHC_PACKAGE_PATH" $ packagePath ld) (\() -> unsetEnv "GHC_PACKAGE_PATH") $ \() -> do
                         putStrLn $ "HOH: " ++ ld
                         doctest args
      Nothing ->
          doctest args
  where
    args = ["-XOverloadedStrings"] ++ flags ++ pkgs ++ module_sources
    packagePath dir = dir ++ "/package.conf.d/"
