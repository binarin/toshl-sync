{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module TinkoffCSVSpec where

import           Data.Decimal
import qualified Data.ByteString as B
import           Data.FileEmbed
import           Data.Time.Calendar (fromGregorian)
import           System.IO
import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)

import           TinkoffCSV
import           Model

main = hspec spec

dec :: Integer -> Integer -> Amount
dec i f = Amount (Decimal 2 $ i * 100 + f) "RUB"


tinkoffSample1 = ReportItem (fromGregorian 2017 05 31)
                            (Just "*2765")
                            (dec (-365) 0)
                            (Just "Супермаркеты")
                            (Just 5411)
                            (Just "Пятерочка")

tinkoffSample2 = ReportItem (fromGregorian 2017 05 31)
                            (Just "*2024")
                            (dec 53854 0)
                            (Just "Финан. услуги")
                            (Just 6012)
                            (Just "Перевод c карты другого банка")

tinkoffSample3 = ReportItem (fromGregorian 2017 05 31)
                            (Just "*3918")
                            (dec (-53854) 0)
                            (Just "Переводы")
                            Nothing
                            (Just "Перевод c карты другого банка")

tinkoffSample4 = ReportItem (fromGregorian 2017 05 22)
                            Nothing
                            (dec 509 0)
                            (Just "Другое")
                            Nothing
                            (Just "Вознаграждение за операции покупок")

tinkoffSampleRaw :: B.ByteString
tinkoffSampleRaw = $(embedFile "tests/tinkoff-sample.csv")

spec :: Spec
spec =
  describe "readCsv" $ do
    it "it should correctly parse tinkoff csv" $ do
       readCsv tinkoffSampleRaw `shouldBe` Right [tinkoffSample1, tinkoffSample2, tinkoffSample3, tinkoffSample4]
