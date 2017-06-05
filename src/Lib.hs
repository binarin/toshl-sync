{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib where

import qualified Data.Map as M
import Data.Default
import Data.Vector (Vector)
import Text.RawString.QQ
import Data.Maybe
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Control.Lens
import Data.Csv
import Data.Char (ord, chr)
import qualified Data.ByteString.Lazy as BL

-- data ReconcileResult = ReconcileResult { _reconcileResultMatched :: [([Transaction], [TCSTrn])]
--                                        , _reconcileResultNotRecorded :: [TCSTrn]
--                                        , _reconcileResultUnknown :: [Transaction]
--                                        }

-- instance Default ReconcileResult where
--     def = ReconcileResult undefined undefined undefined

-- makeFields ''ReconcileResult

-- reconcileConfig = ReconcileConfig $ M.fromList

-- type Rule = [Transaction] -> [TCSTrn] -> Maybe ([Transaction], [TCSTrn], ([Transaction], [TCSTrn]))

-- simpleAccountMatcher = undefined
-- testRules = [simpleAccountMatcher "*2765" "tcs-credit"
--             ,simpleAccountMatcher "*0723" "tcs-credit"
--             ,simpleAccountMatcher "*2024" "binarin-tcs"]

-- reconcile :: [Rule] -> [Transaction] -> [TCSTrn] -> ReconcileResult
-- reconcile cfg toshl bank = go toshl bank def
--   where
--     go [] bankTrns rr = rr & notRecorded %~ (++ bankTrns)
--     go toshlTrns [] rr = rr & unknown %~ (++ toshlTrns)
--     go toshlTrns@(t:ts) bankTrns rr =
--         case evalRules toshlTrns bankTrns of
--           Just (toshlTrns', bankTrns', match) ->
--               go toshlTrns' bankTrns' (rr & matched %~ (match:))
--           Nothing ->
--               go ts bankTrns (rr & unknown %~ (t:))
--     evalRules = undefined


-- csvSample :: BL.ByteString
-- csvSample = BL.fromStrict $ encodeUtf8 [r|"Date","Account","Category","Tags","Expense amount","Income amount","Currency","In main currency","Main currency","Description"
-- 5/1/17,tcs-credit,Связь,ucanet,555.00,0,RUB,555.00,RUB,
-- 5/15/17,binarin-alfa,Salary,Mirantis,0,"26,000.00",RUB,"26,000.00",RUB,
-- |]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
