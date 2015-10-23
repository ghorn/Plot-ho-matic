{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SetHo
       ( runSetter
         -- * re-exported for convenience
       , Lookup
       ) where

import qualified GHC.Stats

import qualified Control.Concurrent as CC
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import Text.Printf ( printf )
--import System.Glib.Signals ( on )

import Accessors

import SetHo.LookupTree ( GraphInfo(..), newLookupTreeview, makeOptionsWidget )


-- | fire up the the GUI
runSetter :: forall a . Lookup a => a -> IO a -> (a -> IO ()) -> IO ()
runSetter initialValue refresh commit = do
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
  _ <- Gtk.onDestroy win killEverything

  --------------- main widget -----------------
  buttonCommit <- Gtk.buttonNewWithLabel "commit"
  buttonRefresh <- Gtk.buttonNewWithLabel "refresh"
  Gtk.widgetSetTooltipText buttonCommit (Just "SET ME SET ME GO HEAD DO IT COME ON SET ME")

  msgStore <- Gtk.listStoreNew [initialValue]
  let newMessage :: a -> IO ()
      newMessage next =
        -- grab the time and counter
        Gtk.postGUIAsync $ do
          size <- Gtk.listStoreGetSize msgStore
          if size == 0
            then Gtk.listStorePrepend msgStore next
            else Gtk.listStoreSetValue msgStore 0 next

  -- mvar with all the user input
  graphInfoMVar <- CC.newMVar GraphInfo { giXScaling = True
                                        , giXRange = Nothing
                                        , giValue = initialValue
                                        } :: IO (CC.MVar (GraphInfo a))

  -- the options widget
  optionsWidget <- makeOptionsWidget graphInfoMVar
  options <- Gtk.expanderNew "options"
  Gtk.set options [ Gtk.containerChild := optionsWidget
                  , Gtk.expanderExpanded := False
                  ]

  -- the signal selector
  (treeview, getLatestStaged) <- newLookupTreeview initialValue msgStore
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
    , Gtk.containerChild := options
    , Gtk.boxChildPacking options := Gtk.PackNatural
    , Gtk.containerChild := treeviewExpander
    , Gtk.boxChildPacking treeviewExpander := Gtk.PackGrow
    ]

  _ <- Gtk.onClicked buttonCommit $ do
       val <- getLatestStaged
       commit val
       
  _ <- Gtk.onClicked buttonRefresh $ do
    newVal <- refresh
    newMessage newVal

-- add widget to window and show
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI
