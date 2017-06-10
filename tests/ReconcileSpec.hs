{-# LANGUAGE OverloadedStrings #-}
module ReconcileSpec where

import Data.Decimal
import Data.Time.Calendar
import Data.Text
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Model
import Reconcile

main = hspec spec

mkTrn :: Text -> Integer -> Transaction
mkTrn account amount = Transaction
                          (fromGregorian 2017 5 31)
                          account
                          (Category "Еда и напитки")
                          (Amount (Decimal 2 $ -amount * 100) "RUB")
                          ["binarin"]
                          (Just "Пятерочка")

mkRep :: Text -> Integer -> ReportItem
mkRep account amount = ReportItem (fromGregorian 2017 05 31)
                                  (Just account)
                                  (Amount (Decimal 2 $ -amount * 100) "RUB")
                                  (Just "Супермаркеты")
                                  (Just 5411)
                                  (Just "Пятерочка")

spec :: Spec
spec = do
  describe "reconcile" $ do
    it "should treat every ReportItem as un-recorded when there is no rules" $ do
      reconcile [] [] [mkRep "*1234" 100] `shouldBe` ReconcileResult [] [mkRep "*1234" 100] []
    it "should treate every Transaction as un-matched when there is no rules" $ do
      reconcile [] [mkTrn "tcs-credit" 200] [] `shouldBe` ReconcileResult [] [] [mkTrn "tcs-credit" 200]
    it "should properly perform simplest matching" $ do
      let trn1 = mkTrn "tcs-credit" 155
          rep1 = mkRep "*1234" 155
          got = reconcile [simpleAccountMatcher "tcs-credit" "*1234"] [trn1] [rep1]
          expect = ReconcileResult [([trn1], [rep1])] [] []
      got `shouldBe` expect
