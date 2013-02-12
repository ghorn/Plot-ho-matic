{-# OPTIONS_GHC -Wall #-}
{-# Language OverloadedStrings #-}

module Plotter ( runPlotter ) where

import qualified Control.Concurrent as C
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import System.Remote.Monitoring ( forkServer )

import PlotTypes ( GraphInfo(..), PContainer(..), VarInfo(..), clearVarInfo )
import PlotChart

data ListViewInfo = ListViewInfo String (C.MVar PContainer) Bool

runPlotter :: [VarInfo] -> [C.ThreadId] -> IO ()
runPlotter infos backgroundThreadsToKill = do
  _ <- forkServer "localhost" 8000
  _ <- Gtk.initGUI
 
  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "Plot-ho-matic"
                   ]
  
  graphWindowsToBeKilled <- C.newMVar []
  let myQuit = do
        gws <- C.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ C.killThread backgroundThreadsToKill
        Gtk.mainQuit
  _ <- Gtk.onDestroy win myQuit

  buttonQuit <- Gtk.buttonNewWithLabel "QUIT"
  _ <- Gtk.onClicked buttonQuit (Gtk.widgetDestroy win)

  buttonClear <- Gtk.buttonNewWithLabel "clear all values"
  _ <- Gtk.onClicked buttonClear $ mapM_ clearVarInfo infos

  -- button to create a new graph
  buttonNewGraph <- Gtk.buttonNewWithLabel "moar graph"
  _ <- Gtk.onClicked buttonNewGraph (newGraph graphWindowsToBeKilled infos)

  -- vbox to hold buttons
  vbox <- Gtk.vBoxNew False 4
--  Gtk.containerAdd win button
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.set vbox [ Gtk.containerChild := buttonQuit
               , Gtk.containerChild := buttonNewGraph
               , Gtk.containerChild := buttonClear
               ]

  Gtk.widgetShowAll win
  
  Gtk.mainGUI


-- make a new graph window
newGraph :: C.MVar [Gtk.Window] -> [VarInfo] -> IO ()
newGraph graphWindowsToBeKilled infos = do
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "I am a graph"
                   ]

  -- create a new tree model
  model <- Gtk.listStoreNew $ map (\(VarInfo st pc) -> ListViewInfo st pc False) infos
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

  Gtk.cellLayoutSetAttributes col1 renderer1 model $ \(ListViewInfo name _ _) -> [ Gtk.cellText := name]
  Gtk.cellLayoutSetAttributes col2 renderer2 model $ \(ListViewInfo _ _ marked) -> [ Gtk.cellToggleActive := marked ]

  _ <- Gtk.treeViewAppendColumn treeview col1
  _ <- Gtk.treeViewAppendColumn treeview col2

  -- update the model when the toggle buttons are activated
  graphInfoMVar <- C.newMVar (GraphInfo [])
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    -- toggle the check mark
    let (i:_) = Gtk.stringToTreePath pathStr
    (ListViewInfo n p val) <- Gtk.listStoreGetValue model i
    Gtk.listStoreSetValue model i (ListViewInfo n p (not val))

    -- update the graph information
    varinfos' <- Gtk.listStoreToList model
    let newGraphInfo = GraphInfo [(str, pc) | (ListViewInfo str pc marked) <- varinfos', marked]
    _ <- C.swapMVar graphInfoMVar newGraphInfo
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

  C.modifyMVar_ graphWindowsToBeKilled (return . (win:))

  Gtk.widgetShowAll win
  
  return ()

animationWaitTime :: Int
animationWaitTime = 3
