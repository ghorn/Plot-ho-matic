{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.MultiSelectWidget
       ( multiSelectWidget
       ) where

import Control.Monad ( unless, void, zipWithM )
import Control.Monad.IO.Class ( liftIO )
import qualified Control.Concurrent as CC
import Data.Default.Class ( def )
import Data.Maybe ( fromMaybe )
import qualified Data.IORef as IORef
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import System.Exit ( exitFailure )
import System.Glib.Signals ( on )
import Prelude

import PlotHo.GraphWidget ( newGraph, toElement' )
import PlotHo.PlotTypes ( Channel(..), Element(..), Element'(..), PlotterOptions(..) )
import PlotHo.SignalSelector ( SignalSelector(..), Selector(..), newMultiSignalSelectorArea )


-- | fire up the the GUI
multiSelectWidget :: Maybe PlotterOptions -> [Channel] -> IO (CC.MVar [Gtk.Window])
multiSelectWidget mplotterOptions channels = do
  let plotterOptions = fromMaybe def mplotterOptions

  unless CC.rtsSupportsBoundThreads $ do
    putStr $ unlines
      [ "Plot-ho-matic requires the threaded RTS."
      ,  "Please recompile your program with the -threaded GHC option."
      , "Either add \"ghc-options: -threaded\" to your cabal file "
      , "or use the -threaded flag when calling GHC from the command line."
      ]
    void exitFailure

  void Gtk.initGUI

  -- start the main window
  win <- Gtk.windowNew
  void $ Gtk.set win
    [ Gtk.containerBorderWidth := 8
    , Gtk.windowTitle := "Plot-ho-matic Multi Selector Deluxe"
    ]

  -- on close of main, kill all the windows and threads
  graphWindowsToBeKilled <- CC.newMVar []
  CC.modifyMVar_ graphWindowsToBeKilled (return . (win:))

  let killEverything :: IO ()
      killEverything = do
        Gtk.mainQuit
  void $ on win Gtk.deleteEvent $ liftIO (killEverything >> return False)

  --------------- main widget -----------------

  -- Multi Signal Selector
  elements <- zipWithM (\k (Channel c) -> Element <$> toElement' k c) [0..] channels
  multiSignalSelector <- newMultiSignalSelectorArea elements 3

  -- refresh signal selector
  buttonRefresh <- Gtk.buttonNewWithLabel "refresh"
  void $ on buttonRefresh Gtk.buttonActivated (rebuildSignals elements multiSignalSelector)

  buttonSpawnPrefilledGraphs <- Gtk.buttonNewWithLabel "gen prefilled graphs"
  void $ on buttonSpawnPrefilledGraphs Gtk.buttonActivated $ do
    let genGraph selector = do
          let signalSelector =
                SignalSelector
                { ssTreeView = ssTreeView multiSignalSelector
                , ssTreeStore = ssTreeStore multiSignalSelector
                , ssSelectors = selector
                }
          graphWin <- newGraph plotterOptions channels (Just signalSelector)
          -- add this window to the list to be killed on exit
          CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))

    mapM_ genGraph (ssSelectors multiSignalSelector)
    Gtk.mainQuit
    Gtk.widgetDestroy win

  treeviewScroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.set treeviewScroll [Gtk.widgetVExpand := True] -- make sure it expands vertically
  Gtk.containerAdd treeviewScroll (ssTreeView multiSignalSelector)
  Gtk.set treeviewScroll
    [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
    , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
    ]
  treeviewExpander <- Gtk.expanderNew "sig"
  Gtk.set treeviewExpander
    [ Gtk.containerChild := treeviewScroll
    , Gtk.expanderExpanded := True
    ]

  -- vbox to hold everything
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox $
    [ Gtk.containerChild := buttonSpawnPrefilledGraphs
    , Gtk.boxChildPacking buttonSpawnPrefilledGraphs := Gtk.PackNatural
    , Gtk.containerChild := buttonRefresh
    , Gtk.boxChildPacking buttonRefresh := Gtk.PackNatural
    , Gtk.containerChild := treeviewExpander
    , Gtk.boxChildPacking treeviewExpander := Gtk.PackGrow
    ]

  void $ Gtk.widgetSetSizeRequest vbox 20 200

  -- add widget to window and show
  void $ Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI
  -- Pass graphs back to main program
  return graphWindowsToBeKilled

rebuildSignals :: [Element] -> SignalSelector [Selector] -> IO ()
rebuildSignals elements signalSelector = do
  let stageDataFromElement :: forall a . Element' a -> IO ()
      stageDataFromElement element = do
        let msgStore = eMsgStore element
        -- get the latest data, just block if they're not available
        mdatalog <- CC.takeMVar msgStore
        case mdatalog of
          -- no data yet, do nothing
          Nothing -> CC.putMVar msgStore mdatalog
          Just (datalog, msignalTree) -> do
            case msignalTree of
              -- No new signal tree, no action necessary
              Nothing -> return ()
              -- If there is a new signal tree, we have to merge it with the old one.
              Just newSignalTree -> do
                let rebuilds = sRebuildSignalTree <$> (ssSelectors signalSelector)
                mapM_ (\x -> x element newSignalTree) rebuilds

            -- write the data to the IORef so that the getters get the right stuff
            IORef.writeIORef (ePlotValueRef element) datalog

            -- Put the data back. Put Nothing to signify that the signal tree is up to date.
            CC.putMVar msgStore (Just (datalog, Nothing))

    -- stage the values
  mapM_ (\(Element e) ->  stageDataFromElement e) elements