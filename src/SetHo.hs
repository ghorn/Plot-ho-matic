{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language PackageImports #-}

-- | This is an experimental and unstable interface for
-- generating a GUI for getting/setting options.
module SetHo
       ( SetHoConfig(..), defaultSetHoConfig
       , runSetter
       ) where

import qualified GHC.Stats

import Accessors.Dynamic ( DTree )
import qualified Control.Concurrent as CC
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy as BSL
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Serialize ( encodeLazy, decodeLazy )
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import Text.Printf ( printf )
import System.Glib.Signals ( on )

import SetHo.LookupTree ( newLookupTreeview )

data SetHoConfig
  = SetHoConfig
    { enableAutoCommit :: Bool
    }

defaultSetHoConfig :: SetHoConfig
defaultSetHoConfig =
  SetHoConfig
  { enableAutoCommit = True
  }

-- | fire up the the GUI
runSetter :: Maybe SetHoConfig -> String -> DTree -> IO (Maybe (Int, DTree)) -> (Int -> IO ()) -> (Int -> DTree -> IO ()) -> IO ()
runSetter mconfig rootName initialValue userPollForNewMessage sendRequest userCommit = do
  let config = case mconfig of
        Just r -> r
        Nothing -> defaultSetHoConfig
  statsEnabled <- GHC.Stats.getGCStatsEnabled

  counterRef <- newIORef 0
  upstreamCounterRef <- newIORef Nothing

  _ <- Gtk.initGUI
  _ <- Gtk.timeoutAddFull (CC.yield >> return True) Gtk.priorityDefault 50

  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "set-ho-matic"
                   ]

  statsLabel <- Gtk.labelNew (Nothing :: Maybe String)
  let makeStatsMessage = do
        statsMsg <- if statsEnabled
               then do
                 stats <- GHC.Stats.getGCStats
                 return $ printf "The current memory usage is %.2f MB"
                   ((realToFrac (GHC.Stats.currentBytesUsed stats) :: Double) /(1024*1024))
               else return "(enable GHC statistics with +RTS -T)"
        counter <- readIORef counterRef
        mupstreamCounter <- readIORef upstreamCounterRef
        let upstreamCount = case mupstreamCounter of
              Nothing -> "?"
              Just r -> show r
            counterMsg = "editing: " ++ show counter ++ " | upstream: " ++ upstreamCount
        return $ "Welcome to set-ho-matic!\n" ++ statsMsg ++ "\n" ++ counterMsg

      statsWorker = do
        CC.threadDelay 500000
        msg <- makeStatsMessage
        Gtk.postGUISync $ Gtk.labelSetText statsLabel msg
        statsWorker

  statsThread <- CC.forkIO statsWorker
  -- on close, kill all the windows and threads
  graphWindowsToBeKilled <- CC.newMVar []

--  channels <- execPlotter plotterMonad
--  let windows = map csMkChanEntry channels
--
--  chanWidgets <- mapM (\x -> x graphWindowsToBeKilled) windows

  let killEverything = do
        CC.killThread statsThread
        _gws <- CC.readMVar graphWindowsToBeKilled
--        mapM_ Gtk.widgetDestroy gws
--        mapM_ csKillThreads channels
        Gtk.mainQuit
  _ <- on win Gtk.deleteEvent $ liftIO (killEverything >> return False)

  --------------- main widget -----------------
  buttonCommit <- Gtk.buttonNewWithLabel "commit"
  buttonRefresh <- Gtk.buttonNewWithLabel "refresh"
  buttonTakeUpstream <- Gtk.buttonNewWithLabel "take upstream"
  Gtk.widgetSetTooltipText buttonCommit
    (Just "SET ME SET ME GO HEAD DO IT COME ON SET ME")

  mbuttonAutoCommit <-
    if enableAutoCommit config
    then do buttonAutoCommit <- Gtk.checkButtonNewWithLabel "auto-commit"
            Gtk.widgetSetTooltipText buttonAutoCommit
              (Just "Send settings upstream as soon as any value is changed")
            return (Just buttonAutoCommit)
    else return Nothing

  -- the options widget
  options <- Gtk.expanderNew "options"
  let mautocommitChild = case mbuttonAutoCommit of
        Nothing -> []
        Just buttonAutoCommit -> [Gtk.containerChild := buttonAutoCommit]
  Gtk.set options $ mautocommitChild ++ [Gtk.expanderExpanded := True]

  -- how to commit
  let commit val = do
        counter <- readIORef counterRef
        putStrLn $ "sending settings update " ++ show counter
        writeIORef counterRef (1 + counter)
        makeStatsMessage >>= Gtk.labelSetText statsLabel
        userCommit counter val

  -- the signal selector
  let getAutoCommitStatus = case mbuttonAutoCommit of
        Nothing -> return False
        Just buttonAutoCommit -> Gtk.toggleButtonGetActive buttonAutoCommit
  (treeview, getLatestStaged, receiveNewUpstream, takeLatestUpstream, loadFromFile) <-
    newLookupTreeview rootName initialValue getAutoCommitStatus commit

  treeviewScroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.set treeviewScroll [Gtk.widgetVExpand := True] -- make sure it expands vertically
  Gtk.containerAdd treeviewScroll treeview
  Gtk.set treeviewScroll
    [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
    , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
    ]

  treeviewExpander <- Gtk.expanderNew "signals"
  Gtk.set treeviewExpander
    [ Gtk.containerChild := treeviewScroll
    , Gtk.expanderExpanded := True
    ]

  let menuBarDescription =
        [ ("_File", [ ("Load", Just (loadFile win loadFromFile))
                    , ("Save", Just (saveFile win getLatestStaged))
                    , ("_Quit", Just Gtk.mainQuit)
                    ]
          )
        , ("Help",  [ ("_Help", Just (help win))
                    ]
          )
        ]

  menuBar <- createMenuBar menuBarDescription

  -- vbox to hold buttons and list of channel
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox
    [ Gtk.containerChild := menuBar
    , Gtk.boxChildPacking menuBar := Gtk.PackNatural
    , Gtk.containerChild := statsLabel
    , Gtk.boxChildPacking statsLabel := Gtk.PackNatural
    , Gtk.containerChild := buttonCommit
    , Gtk.boxChildPacking buttonCommit := Gtk.PackNatural
    , Gtk.containerChild := buttonRefresh
    , Gtk.boxChildPacking buttonRefresh := Gtk.PackNatural
    , Gtk.containerChild := buttonTakeUpstream
    , Gtk.boxChildPacking buttonTakeUpstream := Gtk.PackNatural
    , Gtk.containerChild := options
    , Gtk.boxChildPacking options := Gtk.PackNatural
    , Gtk.containerChild := treeviewExpander
    , Gtk.boxChildPacking treeviewExpander := Gtk.PackGrow
    ]

  _ <- on buttonCommit Gtk.buttonActivated $ (getLatestStaged >>= commit)

  _ <- on buttonRefresh Gtk.buttonActivated $ do
    counter <- readIORef counterRef
    putStrLn $ "sending settings request " ++ show counter
    sendRequest counter

  _ <- on buttonTakeUpstream Gtk.buttonActivated takeLatestUpstream

  let pollForNewMessage = do
        mmsg <- userPollForNewMessage
        case mmsg of
          Nothing -> return ()
          Just (upstreamCounter, newVal) -> do
            putStrLn $ "received settings update " ++ show upstreamCounter
            writeIORef upstreamCounterRef (Just upstreamCounter)
            makeStatsMessage >>= Gtk.labelSetText statsLabel
            receiveNewUpstream newVal

  _ <- Gtk.timeoutAddFull (pollForNewMessage >> return True) Gtk.priorityDefault 300

  -- add widget to window and show
  _ <- Gtk.set win [ Gtk.windowTitle := "set-ho-matic 2000"
                   , Gtk.containerChild := vbox
                   ]
  Gtk.widgetShowAll win
  Gtk.mainGUI

createMenuBar :: [( String

                  , [( String
                     , Maybe (IO ())
                     )]
                  )] -> IO Gtk.MenuBar
createMenuBar descr
  = do bar <- Gtk.menuBarNew
       mapM_ (createMenu bar) descr
       return bar
  where
    createMenu bar (name,items)
        = do menu <- Gtk.menuNew
             item <- menuItemNewWithLabelOrMnemonic name
             Gtk.menuItemSetSubmenu item menu
             Gtk.menuShellAppend bar item
             mapM_ (createMenuItem menu) items
    createMenuItem menu (name,action)
        = do item <- menuItemNewWithLabelOrMnemonic name
             Gtk.menuShellAppend menu item
             case action of
               Just act -> on item Gtk.menuItemActivate act
               Nothing  -> on item Gtk.menuItemActivate (return ())
    menuItemNewWithLabelOrMnemonic name
        | elem '_' name = Gtk.menuItemNewWithMnemonic name
        | otherwise     = Gtk.menuItemNewWithLabel name


loadFile :: Gtk.Window -> (DTree -> IO ()) -> IO ()
loadFile win loadData = do
  dialog <- Gtk.fileChooserDialogNew (Just "load settings") (Just win)
            Gtk.FileChooserActionOpen
            [ ("Cancel", Gtk.ResponseCancel)
            , ("Load", Gtk.ResponseAccept)
            ]
  responseId <- Gtk.dialogRun dialog
  case responseId of
    Gtk.ResponseAccept -> do
      mfileName <- Gtk.fileChooserGetFilename dialog
      case mfileName of
        Nothing -> error "fileChooserGetFileName failed on ResponseAccept"
        Just path -> do
          bs <- BSL.readFile path
          case decodeLazy bs of
            Left msg -> errorMsg win ("error decoding " ++ show path ++ "\n" ++ msg)
            Right r -> loadData r >> putStrLn ("loaded " ++ show path)
    _ -> return ()
  Gtk.widgetDestroy dialog


saveFile :: Gtk.Window -> IO DTree -> IO ()
saveFile win getLatestStaged = do
  dialog <- Gtk.fileChooserDialogNew (Just "save settings") (Just win)
            Gtk.FileChooserActionSave
            [ ("Cancel", Gtk.ResponseCancel)
            , ("Save", Gtk.ResponseAccept)
            ]
  Gtk.fileChooserSetDoOverwriteConfirmation dialog True
  responseId <- Gtk.dialogRun dialog
  case responseId of
    Gtk.ResponseAccept -> do
      mfileName <- Gtk.fileChooserGetFilename dialog
      case mfileName of
        Nothing -> error "fileChooserGetFileName failed on ResponseAccept"
        Just fileName -> do
          staged <- getLatestStaged
          BSL.writeFile fileName (encodeLazy staged)
          putStrLn $ "saved settings in " ++ show fileName
    _ -> return ()
  Gtk.widgetDestroy dialog

help :: Gtk.Window -> IO ()
help win = do
  dialog <- Gtk.messageDialogNew (Just win) [Gtk.DialogDestroyWithParent]
            Gtk.MessageInfo Gtk.ButtonsOk
            "There is no help for you here."
  _responseId <- Gtk.dialogRun dialog
  Gtk.widgetDestroy dialog

errorMsg :: Gtk.Window -> String -> IO ()
errorMsg win message = do
  dialog <- Gtk.messageDialogNew (Just win) [Gtk.DialogDestroyWithParent]
            Gtk.MessageError Gtk.ButtonsOk
            message
  _responseId <- Gtk.dialogRun dialog
  Gtk.widgetDestroy dialog
