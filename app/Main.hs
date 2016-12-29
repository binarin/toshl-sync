module Main where

import Protolude
import Config
import Accounts

main :: IO ()
main = do
  cfg <- readConfig
  accs <- getAccounts cfg
  -- tags <- getTags cfg
  -- cats <- getCategories cfg
  -- parse tinkoff csv and decide on date range
  -- determine toshl accounts based on card numbers in tinkoff csv
  -- toshlList <-
  return ()
