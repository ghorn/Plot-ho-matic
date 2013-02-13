{-# OPTIONS_GHC -Wall #-}

module Plotter ( runPlotter ) where

import qualified Control.Concurrent as CC
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )

import PlotTypes ( Channel(..), PbPrim, GraphInfo(..) )
import PlotChart

data ListViewInfo a = ListViewInfo { lviName :: String
                                   , lviGetter :: a -> PbPrim
                                   , lviMarked :: Bool
                                   , lviMaxToShow :: Int
                                   }


channelWidget :: [Channel] -> CC.MVar [Gtk.Window] -> IO Gtk.TreeView
channelWidget channels graphWindowsToBeKilled = do
  -- create a new tree model
  model <- Gtk.listStoreNew channels
  treeview <- Gtk.treeViewNewWithModel model

  Gtk.treeViewSetHeadersVisible treeview True

  -- add some columns
  col0 <- Gtk.treeViewColumnNew
  col1 <- Gtk.treeViewColumnNew
  col2 <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle col0 "channel"
  Gtk.treeViewColumnSetTitle col1 "history"
  Gtk.treeViewColumnSetTitle col2 "new"

  renderer0 <- Gtk.cellRendererTextNew
  renderer1 <- Gtk.cellRendererTextNew
  renderer2 <- Gtk.cellRendererToggleNew

  Gtk.cellLayoutPackStart col0 renderer0 True
  Gtk.cellLayoutPackStart col1 renderer1 True
  Gtk.cellLayoutPackStart col2 renderer2 True

  Gtk.cellLayoutSetAttributes col0 renderer0 model $ \chan -> [ Gtk.cellText := chanName chan]
  Gtk.cellLayoutSetAttributes col1 renderer1 model $ \_ -> [ Gtk.cellText := "???"]
  Gtk.cellLayoutSetAttributes col2 renderer2 model $ \_ -> [ Gtk.cellToggleActive := False]

  _ <- Gtk.treeViewAppendColumn treeview col0
  _ <- Gtk.treeViewAppendColumn treeview col1
  _ <- Gtk.treeViewAppendColumn treeview col2

  -- spawn new graph when checkbox is clicked
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    let (i:_) = Gtk.stringToTreePath pathStr
    channel <- Gtk.listStoreGetValue model i
    graphWin <- newGraph channel
    -- kill this window when the main window is killed
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))

  return treeview




runPlotter :: [Channel] -> [CC.ThreadId] -> IO ()
runPlotter channels backgroundThreadsToKill = do
  _ <- Gtk.initGUI
 
  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "Plot-ho-matic"
                   ]

  -- kill all the threads and widgets on close
  graphWindowsToBeKilled <- CC.newMVar []
  let myQuit = do
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ CC.killThread backgroundThreadsToKill
        Gtk.mainQuit
  _ <- Gtk.onDestroy win myQuit

  --------------- main widget -----------------
  buttonQuit <- Gtk.buttonNewWithLabel "QUIT"
  _ <- Gtk.onClicked buttonQuit (Gtk.widgetDestroy win)

  buttonClear <- Gtk.buttonNewWithLabel "clear all values"
  _ <- Gtk.onClicked buttonClear $ putStrLn "you pressed \"clear\"" -- mapM_ clearVarInfo infos

  chanWidget <- channelWidget channels graphWindowsToBeKilled

  -- vbox to hold buttons
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox [ Gtk.containerChild := buttonQuit
               , Gtk.containerChild := buttonClear
               , Gtk.containerChild := chanWidget
               ]
  --------------------------------------------

  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI


-- make a new graph window
newGraph :: Channel -> IO Gtk.Window
newGraph (Channel {chanGetters = changetters, chanSeq = chanseq}) = do
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "I am a graph"
                   ]

  -- create a new tree model
  let lviInit (name,getter) =
        ListViewInfo { lviName = name
                     , lviGetter = getter
                     , lviMarked = False
                     , lviMaxToShow = 100
                     }
  model <- Gtk.listStoreNew $ map lviInit changetters
  treeview <- Gtk.treeViewNewWithModel model

  Gtk.treeViewSetHeadersVisible treeview True

  -- add three columns
  col1 <- Gtk.treeViewColumnNew
  col2 <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle col1 "name"
  Gtk.treeViewColumnSetTitle col2 "visible?"

  renderer1 <- Gtk.cellRendererTextNew
  renderer2 <- Gtk.cellRendererToggleNew

  Gtk.cellLayoutPackStart col1 renderer1 True
  Gtk.cellLayoutPackStart col2 renderer2 True

  Gtk.cellLayoutSetAttributes col1 renderer1 model $ \lvi -> [ Gtk.cellText := lviName lvi]
  Gtk.cellLayoutSetAttributes col2 renderer2 model $ \lvi -> [ Gtk.cellToggleActive := lviMarked lvi]

  _ <- Gtk.treeViewAppendColumn treeview col1
  _ <- Gtk.treeViewAppendColumn treeview col2

  -- update the model when the toggle buttons are activated
  numToDrawMv <- CC.newMVar 100
  graphInfoMVar <- CC.newMVar (GraphInfo chanseq numToDrawMv [])
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    -- toggle the check mark
    let (i:_) = Gtk.stringToTreePath pathStr
    lvi0 <- Gtk.listStoreGetValue model i
    Gtk.listStoreSetValue model i (lvi0 {lviMarked = not (lviMarked lvi0)})

    -- update the graph information
    lvis <- Gtk.listStoreToList model
    let newGraphInfo = GraphInfo chanseq numToDrawMv [(lviName lvi, lviGetter lvi) | lvi <- lvis, lviMarked lvi]
    _ <- CC.swapMVar graphInfoMVar newGraphInfo
    return ()

  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  _ <- Gtk.widgetSetSizeRequest chartCanvas 250 250
  _ <- Gtk.onExpose chartCanvas $ const (updateCanvas graphInfoMVar chartCanvas)
  _ <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw chartCanvas
      return True)
    Gtk.priorityDefaultIdle animationWaitTime

  -- vbox to hold treeview and gl drawing
  hbox <- Gtk.hBoxNew False 4
  _ <- Gtk.set win [ Gtk.containerChild := hbox ]
  Gtk.set hbox [ Gtk.containerChild := treeview
               , Gtk.containerChild := chartCanvas
               , Gtk.boxChildPacking treeview := Gtk.PackNatural
               ]

  Gtk.widgetShowAll win
  return win


animationWaitTime :: Int
animationWaitTime = 3
