{-# OPTIONS_GHC -Wall #-}

module Plotter ( runPlotter ) where

import qualified Control.Concurrent as CC
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )

import PlotTypes ( Channel(..), PbPrim, GraphInfo(..) )
import PlotChart

data ListViewInfo a = ListViewInfo { lviName :: String
--                                   , lviContainer :: CC.MVar PContainer
                                   , lviGetter :: a -> PbPrim
                                   , lviMarked :: Bool
                                   , lviMaxToShow :: Int
                                   }


runPlotter :: Channel a -> [CC.ThreadId] -> IO ()
runPlotter channel backgroundThreadsToKill = do
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

  -- button to create a new graph
  buttonNewGraph <- Gtk.buttonNewWithLabel "moar graph"
  _ <- Gtk.onClicked buttonNewGraph (newGraph graphWindowsToBeKilled channel)

  -- vbox to hold buttons
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox [ Gtk.containerChild := buttonQuit
               , Gtk.containerChild := buttonNewGraph
               , Gtk.containerChild := buttonClear
               ]
  --------------------------------------------

  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI


-- make a new graph window
newGraph :: CC.MVar [Gtk.Window] -> Channel a -> IO ()
newGraph graphWindowsToBeKilled channel = do
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
  model <- Gtk.listStoreNew $ map lviInit (chanGetters channel)
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
  graphInfoMVar <- CC.newMVar (GraphInfo (chanSeq channel) numToDrawMv [])
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    -- toggle the check mark
    let (i:_) = Gtk.stringToTreePath pathStr
    lvi0 <- Gtk.listStoreGetValue model i
    Gtk.listStoreSetValue model i (lvi0 {lviMarked = not (lviMarked lvi0)})

    -- update the graph information
    lvis <- Gtk.listStoreToList model
    let newGraphInfo = GraphInfo (chanSeq channel) numToDrawMv [(lviName lvi, lviGetter lvi) | lvi <- lvis, lviMarked lvi]
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

  -- kill this window when the main window is killed
  CC.modifyMVar_ graphWindowsToBeKilled (return . (win:))

  Gtk.widgetShowAll win


animationWaitTime :: Int
animationWaitTime = 3
