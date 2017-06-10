{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CLI where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.Extra (concatMapM)
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Control.Lens
import           Data.Time.Calendar
import           Data.Time.Format
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.Directory (listDirectory)
import           System.FilePath

import           UI
import qualified ToshlCSV
import           Model

class TransactionSource a where
  getAccounts :: a -> IO [Account]
  getTransactions :: a -> Account -> Day -> Day -> IO [Transaction]

data ToshlCSVSource = ToshlCSVSource { reportsDir :: FilePath
                                     } deriving (Show)


-- | Given starting and ending date, return a list of first days of
-- months that are touched by that range.
--
-- >>> rangeToMonths (fromGregorian 2017 05 03) (fromGregorian 2017 08 15)
-- [2017-05-01,2017-06-01,2017-07-01,2017-08-01]
rangeToMonths :: Day -> Day -> [Day]
rangeToMonths from to
  | from > to = []
  | otherwise = fromMonth:(rangeToMonths nextMonthStart to)
  where
    fromMonth = let (fy, fm, fd) = toGregorian from
                in fromGregorian fy fm 1
    nextMonthStart = let (fy, fm, fd) = toGregorian from
                         (ny, nm) = incMonth fy fm
                       in fromGregorian ny nm 1
    incMonth ny 12 = (ny + 1, 1)
    incMonth ny nm = (ny, nm + 1)

instance TransactionSource ToshlCSVSource where
  getAccounts ToshlCSVSource{..} = fmap T.pack <$> listDirectory reportsDir
  getTransactions ToshlCSVSource{..} acc from to = filterRequestedDates <$> concatMapM getMonthData (rangeToMonths from to)
    where
      filterRequestedDates = filter (\trn -> from <= trn^.date && trn^.date <= to)
      getMonthData firstDay = do
          let filename = joinPath [ reportsDir
                                  , T.unpack acc
                                  , formatTime defaultTimeLocale "%Y-%m.csv" firstDay
                                  ]
          putStrLn filename
          csv <- B.readFile filename
          case ToshlCSV.readCsv csv of
            Left err -> do
                           putStrLn err
                           pure []
            Right trns -> pure trns


run :: FilePath -> IO ()
run store = do let database = ToshlCSVSource store
               putStrLn $ show database
               startGUI defaultConfig (setup database)


setup :: ToshlCSVSource -> Window -> UI ()
setup database rootWindow =
    do accounts <- liftIO $ getAccounts database
       liftIO $ putStrLn $ show database
       let showItem item = UI.li #+ [ UI.string $ T.unpack item ]
       list <- UI.ul #+ fmap showItem accounts
       picker <- periodPicker
       UI.set UI.children
              [ list, picker ]
              (getBody rootWindow)
       return ()
