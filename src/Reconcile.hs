{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Reconcile ( reconcile
                 , Rule
                 , simpleAccountMatcher
                 ) where

import           Debug.Trace (trace)
import qualified Data.Text as T
import           Data.List (partition)
import           Data.Default
import           Control.Lens

import           Model

instance Default ReconcileResult where
  def = ReconcileResult [] [] []

reconcile :: [Rule] -> [Transaction] -> [ReportItem] -> ReconcileResult
reconcile rules toshl bank = foldr id initialRR rules
  where
      initialRR = (def & notRecorded .~ bank
                       & unknown .~ toshl)

    -- go rules
    -- where
    --     go [] rr = rr
    --     go (r:rs) rr = case r rr of
    --                      Nothing -> go rs rr
    --                      Just rr' -> go (r:rs) rr'

-- reconcileRule :: Rule -> ReconcileResult -> ReconcileResult
-- reconcileRule rule toshl bank = go (rr^.unknown) (rr^.notRecorded) def
--   where
--     go [] bankTrns rr = rr & notRecorded %~ (++bankTrns)
--     go toshlTrns [] rr = rr & unknown %~ (++toshlTrns)
--     go toshlTrns@(t:ts) bankTrns rr =
--       case evalRules toshlTrns bankTrns of
--         Just (toshlTrns', bankTrns', match) ->
--             go toshlTrns' bankTrns' (rr & matched %~ (match:))
--         Nothing ->
--           go ts bankTrns (rr & unknown %~ (t:))

simpleAccountMatcher :: T.Text -> T.Text -> Rule
simpleAccountMatcher acc card rr = go (rr & notRecorded .~ []) (rr^.notRecorded)
    where
        go :: ReconcileResult -> [ReportItem] -> ReconcileResult
        go rr [] = rr
        go rr (b:bs)
         | b^.cardNo /= Just card = go (rr & notRecorded %~ (b:)) bs
         | otherwise =
           let matcher t = all id [b^.date == t^.date
                                  ,b^.cardNo == Just card
                                  ,b^.amount == t^.amount
                                  ]
               (trnCandidates, trnRemaining) = partition matcher (rr^.unknown)
           in case trnCandidates of
                (t:ts) -> go (rr & matched %~ (([t], [b]):)
                                 & unknown .~ ts ++ trnRemaining) bs
                _ -> go (rr & notRecorded %~ (b:)) bs

