{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language PackageImports #-}

-- | This is an experimental and unstable interface for
-- generating a GUI for getting/setting options.
module SetHo
       ( runSetter
       ) where

import qualified GHC.Stats

import Accessors.Dynamic ( DTree )
import qualified Control.Concurrent as CC
import Control.Monad.IO.Class ( liftIO )
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import Text.Printf ( printf )
import System.Glib.Signals ( on )

import SetHo.LookupTree ( newLookupTreeview )

-- | fire up the the GUI
runSetter :: String -> DTree -> IO (Maybe DTree) -> IO () -> (DTree -> IO ()) -> IO ()
runSetter rootName initialValue userPollForNewMessage sendRequest commit = do
  statsEnabled <- GHC.Stats.getGCStatsEnabled

  _ <- Gtk.initGUI
  _ <- Gtk.timeoutAddFull (CC.yield >> return True) Gtk.priorityDefault 50

  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "set-ho-matic"
                   ]

  statsLabel <- Gtk.labelNew (Nothing :: Maybe String)
  let statsWorker = do
        CC.threadDelay 500000
        msg <- if statsEnabled
               then do
                 stats <- GHC.Stats.getGCStats
                 return $ printf "The current memory usage is %.2f MB"
                   ((realToFrac (GHC.Stats.currentBytesUsed stats) :: Double) /(1024*1024))
               else return "(enable GHC statistics with +RTS -T)"
        Gtk.postGUISync $ Gtk.labelSetText statsLabel ("Welcome to set-ho-matic!\n" ++ msg)
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
  buttonAutoCommit <- Gtk.checkButtonNewWithLabel "auto-commit"
  buttonRefresh <- Gtk.buttonNewWithLabel "refresh"
  buttonTakeUpstream <- Gtk.buttonNewWithLabel "take upstream"
  Gtk.widgetSetTooltipText buttonCommit
    (Just "SET ME SET ME GO HEAD DO IT COME ON SET ME")
  Gtk.widgetSetTooltipText buttonAutoCommit
    (Just "Send settings upstream as soon as any value is changed")

  -- the options widget
  options <- Gtk.expanderNew "options"
  Gtk.set options [ Gtk.containerChild := buttonAutoCommit
                  , Gtk.expanderExpanded := True
                  ]


  -- the signal selector
  (treeview, getLatestStaged, receiveNewUpstream, takeLatestUpstream) <-
    newLookupTreeview rootName initialValue (Gtk.toggleButtonGetActive buttonAutoCommit) commit
  treeviewExpander <- Gtk.expanderNew "signals"
  Gtk.set treeviewExpander
    [ Gtk.containerChild := treeview
    , Gtk.expanderExpanded := True
    ]

  -- vbox to hold buttons and list of channel
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox
    [ Gtk.containerChild := statsLabel
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

  _ <- on buttonCommit Gtk.buttonActivated $ do
    val <- getLatestStaged
    commit val

  _ <- on buttonRefresh Gtk.buttonActivated sendRequest

  _ <- on buttonTakeUpstream Gtk.buttonActivated takeLatestUpstream

  let pollForNewMessage = do
        mmsg <- userPollForNewMessage
        case mmsg of
          Nothing -> return ()
          Just newVal -> receiveNewUpstream newVal

  _ <- Gtk.timeoutAddFull (pollForNewMessage >> return True) Gtk.priorityDefault 300

  -- add widget to window and show
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI
