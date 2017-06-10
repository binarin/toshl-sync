{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Reconcile ( reconcile
                 , Rule
                 , ReconcileResult(..)
                 , simpleAccountMatcher
                 ) where

import           Debug.Trace (trace)
import qualified Data.Text as T
import           Data.List (partition)
import           Data.Default
import           Control.Lens

import           Model

type MatchedTransaction = ([Transaction], [ReportItem])

data ReconcileResult = ReconcileResult { _reconcileResultMatched :: [MatchedTransaction]
                                       , _reconcileResultNotRecorded :: [ReportItem]
                                       , _reconcileResultUnknown :: [Transaction]
                                       } deriving (Eq, Show)

makeFields ''ReconcileResult

instance Default ReconcileResult where
  def = ReconcileResult [] [] []

type Rule = [Transaction] -> [ReportItem] -> Maybe ([Transaction], [ReportItem], MatchedTransaction)

reconcile :: [Rule] -> [Transaction] -> [ReportItem] -> ReconcileResult
reconcile rules toshl bank = go toshl bank def
  where
    go [] bankTrns rr = rr & notRecorded %~ (++bankTrns)
    go toshlTrns [] rr = rr & unknown %~ (++toshlTrns)
    go toshlTrns@(t:ts) bankTrns rr =
      case evalRules toshlTrns bankTrns of
        Just (toshlTrns', bankTrns', match) ->
            go toshlTrns' bankTrns' (rr & matched %~ (match:))
        Nothing ->
          go ts bankTrns (rr & unknown %~ (t:))
    evalRules t b = foldr (\rule next -> case rule t b of
                                           Nothing  -> next
                                           v -> v) Nothing rules

simpleAccountMatcher :: T.Text -> T.Text -> Rule
simpleAccountMatcher acc card trns@(t:ts) banks
  | t^.account /= acc = Nothing
  | otherwise =
    let matcher b = all id [b^.date == t^.date
                           ,b^.cardNo == Just card
                           ,b^.amount == t^.amount]
        (bankCandidates, bankRemaining) = partition matcher banks
    in case bankCandidates of
         (b:bs) -> Just (ts, bs++bankRemaining, ([t], [b]))
         _ -> Just undefined
