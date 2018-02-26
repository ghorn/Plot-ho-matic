{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.Plotter
       ( runPlotter
       ) where

import qualified GHC.Stats

import Control.Monad ( unless, void,  )
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

import PlotHo.GraphWidget ( newGraph )
import PlotHo.PlotTypes ( Channel(..), Channel'(..), PlotterOptions(..) )
import PlotHo.MultiSelectWidget ( multiSelectWidget )

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
    graphWin <- newGraph plotterOptions channels Nothing
    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))

  -- multi signal selector
  -- set the number of graphs
  numGraphsLabel <- Gtk.vBoxNew False 4 >>= labeledWidget "num graphs:"
  numGraphsEntry <- Gtk.entryNew
  Gtk.set numGraphsEntry
    [ Gtk.entryEditable := True
    , Gtk.widgetSensitive := True
    ]
  Gtk.entrySetText numGraphsEntry "3"
  let makeMultiGraphs = do
        txt <- Gtk.get numGraphsEntry Gtk.entryText
        case readMaybe txt :: Maybe Int of
          Nothing ->
            putStrLn ("num graphs: couldn't make an Int out of \"" ++ show txt ++ "\"")
          Just 0  -> putStrLn "numGraphs: must be greater than 0"
          Just k  -> do
            mVarMoreGraphsToKill <- multiSelectWidget mplotterOptions channels k
            moreGraphsToKill <- CC.readMVar mVarMoreGraphsToKill
            CC.modifyMVar_ graphWindowsToBeKilled (return . (moreGraphsToKill++))

  -- make the button
  buttonMultiSelector <- Gtk.buttonNewWithLabel "multigraph"
  void $ on buttonMultiSelector Gtk.buttonActivated $ do
    makeMultiGraphs

  hboxMultiSelector <- Gtk.hBoxNew False 4
  Gtk.set hboxMultiSelector
    [ Gtk.containerChild := numGraphsLabel
    , Gtk.boxChildPacking numGraphsLabel := Gtk.PackNatural
    , Gtk.containerChild := numGraphsEntry
    , Gtk.boxChildPacking numGraphsEntry := Gtk.PackNatural
    , Gtk.containerChild := buttonMultiSelector
    , Gtk.boxChildPacking buttonMultiSelector := Gtk.PackGrow
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
    , Gtk.containerChild := hboxMultiSelector
    , Gtk.boxChildPacking hboxMultiSelector := Gtk.PackNatural
    , Gtk.containerChild := scroll
    ]

  void $ Gtk.widgetSetSizeRequest vbox 20 200

  -- add widget to window and show
  void $ Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI

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
