{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module UI where

import           Data.Time.Calendar
import           Data.IORef
import           Data.Time.Clock (UTCTime(utctDay), getCurrentTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Model

data TimePeriod = Month Day deriving (Show)

defaultTimePeriod :: IO TimePeriod
defaultTimePeriod = do today <- utctDay <$> getCurrentTime
                       let (y, m, _) = toGregorian today
                       pure $ Month $ fromGregorian y m 1

decMonth :: TimePeriod -> TimePeriod
decMonth (Month date) = let (y, m, _) = toGregorian date
                      in clampedDec y m
    where
        clampedDec y 1 = Month $ fromGregorian (y-1) 12 1
        clampedDec y m = Month $ fromGregorian y (m-1) 1

incMonth :: TimePeriod -> TimePeriod
incMonth (Month date) = let (y, m, _) = toGregorian date
                         in clampedInc y m
    where
        clampedInc y 12 = Month $ fromGregorian (y+1) 1 1
        clampedInc y m = Month $ fromGregorian y (m+1) 1


periodPicker :: UI (Element, Behavior TimePeriod)
periodPicker =
    do goLeftButton <- UI.button #+ [ string "<" ]
       goRightButton <- UI.button #+ [ string ">" ]
       initialValue <- liftIO $ defaultTimePeriod
       let changeEvent = unionWith const (decMonth <$ UI.click goLeftButton) (incMonth <$ UI.click goRightButton)
       currentValue <- accumE initialValue changeEvent
       storage <- liftIO $ newIORef initialValue
       currentElement <- string $ show initialValue
       container <- UI.div
       UI.set UI.children [goLeftButton, currentElement, goRightButton] $ pure container
       textValue <- stepper initialValue currentValue
       pure currentElement # sink text (show <$> textValue)
       return $ (container, textValue)
