{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ToshlCSVSpec where

import Test.Hspec
import Data.FileEmbed
import qualified Data.ByteString as B
import Data.Time.Calendar (fromGregorian)

import Model
import ToshlCSV (readCsv)

toshlSample1 = Transaction
                  (fromGregorian 2017 5 1)
                  "tcs-credit"
                  (Category "Благотворительность")
                  (Amount (-635.65) "RUB")
                  ["devzen", "learnopengl"]
                  Nothing

toshlSample2 = Transaction
                  (fromGregorian 2017 5 31)
                  "binarin-tcs"
                  (Category "Salary")
                  (Amount 93854 "RUB")
                  ["Mirantis"]
                  (Just "Май")

toshlSampleRaw :: B.ByteString
toshlSampleRaw = $(embedFile "tests/toshl-sample.csv")

main = hspec spec

spec :: Spec
spec = do
    describe "readCsv" $ do
      it "should correctly parse sample CSV" $ do
        readCsv toshlSampleRaw  `shouldBe` Right [toshlSample1, toshlSample2]
