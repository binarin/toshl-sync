{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens hiding (children, set, (#), element)
import           Control.Monad (void)
import qualified Data.ByteString as B
import           Data.List (intersperse, intercalate)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           TextShow

import           CLI hiding (setup)
import           Model
import           Reconcile
import           TinkoffCSV
import           UI


threepennyConfig = defaultConfig { jsCustomHTML = Just "index.html"
                                 , jsStatic = Just "www"
                                 }

main = do let trnStore = ToshlCSVSource "/home/binarin/personal-workspace/toshl-store"
          transactions <- getTransactions trnStore "tcs-credit" (fromGregorian 2017 05 01) (fromGregorian 2017 05 31)
          let rules = [simpleAccountMatcher "tcs-credit" "*0723"
                      ,simpleAccountMatcher "tcs-credit" "*2765"
                      ]
          report <- TinkoffCSV.readCsv <$> B.readFile "/home/binarin/personal-workspace/toshl-store/reports/2017-05-report-tcs-credit.csv"
          case report of
            Right items -> do let recResult = reconcile rules transactions items
                              startGUI threepennyConfig (setup recResult)
          return ()

mkTabs :: Text -> [(Text, Element)] -> UI Element
mkTabs tabId content =
    do
       container <- UI.div
       menuItems <- sequence $ zipWith menuItem [1..] content
       menu <- UI.div # set UI.class_ "ui tabular menu"
                      #+ map element menuItems
       tabItems <- sequence $ zipWith tabItem [1..] content
       element container #+ (element menu:map element tabItems)
       return container
    where
         tabIdString = T.unpack tabId
         menuItem pos (text, _) = UI.div # set UI.class_ ("item" <> if pos == 1
                                                                       then " active"
                                                                       else "")
                                         # set (attr "data-tab") (tabIdString ++ show pos)
                                         #+ [string $ T.unpack text]
         tabItem pos (text, subElement) =
             UI.div # set UI.class_ ("ui tab" <> if pos == 1
                                                       then " active"
                                                       else "")
                    # set (attr "data-tab") (tabIdString ++ show pos)
                    #+ [element subElement]

data Column a = Column Text (a -> UI Element)
              | Column' Text (a -> [UI Element])

renderTable :: [Column a] -> [a] -> UI Element
renderTable cols xs =
    do thead <- UI.mkElement "thead" #+ [UI.tr #+ (mkHeadCell <$> cols)]
       tbody <- UI.mkElement "tbody" #+ (makeRow <$> xs)
       UI.table # set UI.class_ "ui celled table"
                #+ [element thead, element tbody]
    where
         colName (Column n _) = n
         colName (Column' n _) = n
         makeRow x = UI.tr #+ (mkCell x <$> cols)
         mkCell x (Column _ r) = UI.td #+ [r x]
         mkCell x (Column' _ r) = UI.td #+ r x
         mkHeadCell (Column t _) = UI.th #+ [string $ T.unpack t]
         mkHeadCell (Column' t _) = UI.th #+ [string $ T.unpack t]

mkMatchedTab :: [MatchedTransaction] -> UI Element
mkMatchedTab xs =
    do renderTable [Column' "Store" renderTrns
                   ,Column' "Report" renderReport
                   ] xs
    where
        renderTrns (trns, _) = intercalate [UI.hr] (map renderTrn trns)
        renderReport (_, items) = intercalate [UI.hr] (map renderItem items)
        renderItem it = [reportDateAmount it, UI.br, reportCardCatDesc it]
        renderTrn trn = [dateAmount trn, UI.br, accountTagsDesc trn]
        dateAmount trn = string $ show (trn^.date) <> " - " <> show (trn^.amount)
        accountTagsDesc trn = string $ T.unpack (trn^.account) <> " - " <> tagsStr trn <> " - " <> descStr trn
        tagsStr :: Transaction -> String
        tagsStr trn = intercalate "," (T.unpack <$> (trn^.tags))
        descStr trn = T.unpack $ fromMaybe "" (trn^.description)
        reportDateAmount it = string $ show (it^.date) <> " - " <> show (it^.amount)
        reportCardCatDesc it = string $ intercalate " - " (map (T.unpack . fromMaybe "") [it^.cardNo, it^.categoryName, it^.description])




mkNotRecordedTab :: [ReportItem] -> UI Element
mkNotRecordedTab xs =
    do
       renderTable [Column "Date" (renderShow date)
                   ,Column "Card" (renderOptText cardNo)
                   ,Column "Amount" (renderShow amount)
                   ,Column "Category name" (renderOptText categoryName)
                   ,Column "MCC" (renderOptShow mCC)
                   ,Column "Description" (renderOptText description)
                   ] xs
    where
      renderShow l x = string $ show $ x^.l
      renderOptText l x = string $ T.unpack $ fromMaybe "" (x^.l)
      renderOptShow l x = case (x^.l) of
                            Nothing -> string ""
                            Just card -> string $ show card

mkUnknownTab :: [Transaction] -> UI Element
mkUnknownTab ts =
    do renderTable [Column "Date" (renderShow date)
                   ,Column "Account" (renderAccount . (^.account))
                   ,Column "Target" (renderTarget . (^.target))
                   ,Column "Amount" (renderShow amount)
                   ,Column "Tags" (renderTags . (^.tags))
                   ,Column "Description" (renderDescription . (^.description))
                   ] ts
    where
        renderShow l x = string $ show $ x^.l
        renderAccount acc = string $ T.unpack acc
        renderTarget (Category cat) = string $ "CAT:" <> T.unpack cat
        renderTarget (Account acc) = string $ "ACC:" <> T.unpack acc
        renderTags ts = string $ intercalate "," $ map T.unpack ts
        renderDescription = string . T.unpack . fromMaybe ""

setup :: ReconcileResult -> Window -> UI ()
setup rr win = void $ do
    pure win # set title "Toshl Reconcilation"
    matchedTab <- mkMatchedTab (rr^.matched)
    notRecordedTab <- mkNotRecordedTab (rr^.notRecorded)
    unknownTab <- mkUnknownTab (rr^.unknown)
    tabs <- mkTabs "reconcilation-tab-" [(tabHead "Matched" (rr^.matched), matchedTab)
                                        ,(tabHead "Not recorded" (rr^.notRecorded), notRecordedTab)
                                        ,(tabHead "Unknown" (rr^.unknown), unknownTab)
                                        ]
    getBody win #+ [UI.div # set UI.class_ "ui container" #+ [pure tabs]]
    runFunction $ ffi "$('.tabular.menu .item').tab()"
    pure ()
  where
    tabHead name xs = name <> " (" <> showt (length xs) <> ")"
