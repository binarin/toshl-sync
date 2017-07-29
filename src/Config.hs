module Config (Config(..)) where

import Data.Text (Text)

data Config = Config { toshlKey :: Text
                     , toshlUrl :: Text
                     } deriving (Show)
