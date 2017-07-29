{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad (void)
import           Data.List (sortBy)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (Config)
import qualified Options.Applicative.Text as OptT
import qualified Options.Applicative as Opt
import           Data.Monoid
import Control.Lens ((^.))

import           Config
import           ToshlAPI as API

threepennyConfig = defaultConfig { jsCustomHTML = Just "index.html"
                                 , jsStatic = Just "www"
                                 }

cmdlineParser :: Opt.Parser Config
cmdlineParser = Config <$> OptT.textOption ( Opt.long "toshl-key" )
                       <*> OptT.textOption ( Opt.long "toshl-url"
                                          <> Opt.value "https://api.toshl.com/"
                                          <> Opt.showDefault
                                           )

data AppState = AppState { window :: Window
                         , config :: Config
                         , content :: Element
                         }

main = do config <- Opt.execParser $ Opt.info cmdlineParser mempty
          startGUI threepennyConfig (setup config listAccounts)

setup :: Config -> (AppState -> UI ()) -> Window -> UI ()
setup cfg continuation window = do
    contentContainer <- UI.div # set UI.class_ "ui container"
    getBody window #+ [element contentContainer]
    let state = AppState window cfg contentContainer
    continuation state

getAccounts :: MonadIO m => AppState -> m [Account]
getAccounts st@AppState{..} =
    do accs <- liftIO $ API.get config "accounts" []
       pure $ sortBy (\a b -> (a^.order) `compare` (b^.order)) accs

listAccounts :: AppState -> UI ()
listAccounts st@AppState{..} =
    void $ do accs <- getAccounts st
              accsTable <- renderAccountsTable st accs
              element content # set children [accsTable]
              pure window # UI.set title "Toshl Diff"

renderAccountsTable :: AppState -> [Account] -> UI Element
renderAccountsTable st accs =
    do undefined
       undefined
