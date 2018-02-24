{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.Plotter
       ( runPlotter
       ) where

import qualified GHC.Stats

import Control.Monad ( unless, void, zipWithM )
import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Control.Concurrent as CC
import Data.Default.Class ( def )
import Data.Maybe ( fromMaybe )
import qualified Data.IORef as IORef
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import Text.Printf ( printf )
import Text.Read ( readMaybe )
import System.Exit ( exitFailure )
import System.Glib.Signals ( on )
import Prelude

import PlotHo.GraphWidget ( newGraph, toElement' )
import PlotHo.PlotTypes ( Channel(..), Channel'(..), Element(..), Element'(..), PlotterOptions(..) )
import PlotHo.SignalSelector ( SignalSelector(..), Selector(..), newMultiSignalSelectorArea )

-- | fire up the the GUI
runPlotter :: Maybe PlotterOptions -> [Channel] -> IO ()
runPlotter mplotterOptions channels = do
  let plotterOptions = fromMaybe def mplotterOptions
  statsEnabled <- GHC.Stats.getGCStatsEnabled

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
    , Gtk.windowTitle := "Plot-ho-matic"
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
        Gtk.postGUISync $ Gtk.labelSetText statsLabel ("Welcome to Plot-ho-matic!\n" ++ msg)
        statsWorker

  statsThread <- CC.forkIO statsWorker
  -- on close, kill all the windows and threads
  graphWindowsToBeKilled <- CC.newMVar []

  let killEverything :: IO ()
      killEverything = do
        CC.killThread statsThread
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        Gtk.mainQuit
  void $ on win Gtk.deleteEvent $ liftIO (killEverything >> return False)

  --------------- main widget -----------------
  -- button to spawn a new graph
  buttonSpawnGraph <- Gtk.buttonNewWithLabel "new graph"
  void $ on buttonSpawnGraph Gtk.buttonActivated $ do
    graphWin <- newGraph plotterOptions channels
    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))

  buttonSpawnPrefilledGraphs <- Gtk.buttonNewWithLabel "gen prefilled graphs"
  void $ on buttonSpawnPrefilledGraphs Gtk.buttonActivated $ do
    putStrLn "dummy button" --TODO(Rebecca)
    -- graphWin <- newGraph plotterOptions channels
    -- add this window to the list to be killed on exit
    -- CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))

  -- Multi Signal Selector
  elements <- zipWithM (\k (Channel c) -> Element <$> toElement' k c) [0..] channels
  multiSignalSelector <- newMultiSignalSelectorArea elements 3

  -- refresh signal selector
  buttonRefresh <- Gtk.buttonNewWithLabel "refresh"
  void $ on buttonRefresh Gtk.buttonActivated (rebuildSignals elements multiSignalSelector)

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

  -- clear history / max history widget for each channel
  chanWidgets <- mapM (\(Channel c) -> newChannelWidget c) channels

  -- box to hold list of channels
  channelBox <- Gtk.vBoxNew False 4
  Gtk.set channelBox $
    concatMap (\x -> [ Gtk.containerChild := x
                     , Gtk.boxChildPacking x := Gtk.PackNatural
                     ]) chanWidgets

  -- scroll to hold channel box
  scroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.scrolledWindowAddWithViewport scroll channelBox
  Gtk.set scroll [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
                 , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
                 ]

  -- vbox to hold everything
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox $
    [ Gtk.containerChild := statsLabel
    , Gtk.boxChildPacking statsLabel := Gtk.PackNatural
    , Gtk.containerChild := buttonSpawnGraph
    , Gtk.boxChildPacking buttonSpawnGraph := Gtk.PackNatural
    , Gtk.containerChild := buttonSpawnPrefilledGraphs
    , Gtk.boxChildPacking buttonSpawnPrefilledGraphs := Gtk.PackNatural
    , Gtk.containerChild := buttonRefresh
    , Gtk.boxChildPacking buttonRefresh := Gtk.PackNatural
    , Gtk.containerChild := treeviewExpander
    , Gtk.boxChildPacking treeviewExpander := Gtk.PackGrow
    , Gtk.containerChild := scroll
    ]

  void $ Gtk.widgetSetSizeRequest vbox 20 200

  -- add widget to window and show
  void $ Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI

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


-- the list of channels
newChannelWidget :: Channel' a -> IO Gtk.VBox
newChannelWidget channel = do
  vbox <- Gtk.vBoxNew False 4

  nameBox' <- Gtk.hBoxNew False 4
  nameBox <- labeledWidget (chanName channel) nameBox'

  buttonsBox <- Gtk.hBoxNew False 4

  -- button to clear history
  buttonClearHistory <- Gtk.buttonNewWithLabel "clear history"
  void $ on buttonClearHistory Gtk.buttonActivated $ case chanClearHistory channel of
    Nothing -> putStrLn "not clearing history because that doesn't make sense for this type of data"
    Just clearHistory -> do
      mlatestValue <- CC.takeMVar (chanLatestValueMVar channel)
      case mlatestValue of
        Nothing -> do
          putStrLn "not clearing history because no messages has been received"
          CC.putMVar (chanLatestValueMVar channel) mlatestValue
        Just (latestValue, signalTree) -> do
          putStrLn $ "clearing history for channel " ++ show (chanName channel)
          CC.putMVar (chanLatestValueMVar channel) (Just (clearHistory latestValue, signalTree))

  -- entry to set history length
  maxHistoryEntryAndLabel <- Gtk.hBoxNew False 4
  maxHistoryLabel <- Gtk.vBoxNew False 4 >>= labeledWidget "max history:"
  maxHistoryEntry <- Gtk.entryNew
  Gtk.set maxHistoryEntry
    [ Gtk.entryEditable := True
    , Gtk.widgetSensitive := True
    ]
  Gtk.entrySetText maxHistoryEntry "500"
  let updateMaxHistory = do
        txt <- Gtk.get maxHistoryEntry Gtk.entryText
        let reset = Gtk.entrySetText maxHistoryEntry "(max)"
        case readMaybe txt :: Maybe Int of
          Nothing ->
            putStrLn ("max history: couldn't make an Int out of \"" ++ show txt ++ "\"") >> reset
          Just 0  -> putStrLn ("max history: must be greater than 0") >> reset
          Just k  -> IORef.writeIORef (chanMaxHistory channel) k

  void $ on maxHistoryEntry Gtk.entryActivate updateMaxHistory
  updateMaxHistory


  Gtk.set maxHistoryEntryAndLabel
    [ Gtk.containerChild := maxHistoryLabel
    , Gtk.boxChildPacking maxHistoryLabel := Gtk.PackNatural
    , Gtk.containerChild := maxHistoryEntry
    , Gtk.boxChildPacking maxHistoryEntry := Gtk.PackNatural
    ]


  -- put all the buttons/entries together
  Gtk.set buttonsBox
    [ Gtk.containerChild := buttonClearHistory
    , Gtk.boxChildPacking buttonClearHistory := Gtk.PackNatural
    , Gtk.containerChild := maxHistoryEntryAndLabel
    , Gtk.boxChildPacking maxHistoryEntryAndLabel := Gtk.PackNatural
    ]

  Gtk.set vbox
    [ Gtk.containerChild := nameBox
    , Gtk.boxChildPacking   nameBox := Gtk.PackNatural
    , Gtk.containerChild := buttonsBox
    , Gtk.boxChildPacking   buttonsBox := Gtk.PackNatural
    ]

  return vbox


-- helper to make an hbox with a label
labeledWidget :: Gtk.WidgetClass a => String -> a -> IO Gtk.HBox
labeledWidget name widget = do
  label <- Gtk.labelNew (Just name)
  hbox <- Gtk.hBoxNew False 4
  Gtk.set hbox [ Gtk.containerChild := label
               , Gtk.containerChild := widget
               , Gtk.boxChildPacking label := Gtk.PackNatural
--               , Gtk.boxChildPacking widget := Gtk.PackNatural
               ]
  return hbox
