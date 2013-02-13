{-# OPTIONS_GHC -Wall #-}

module Plotter ( runPlotter ) where

import qualified Control.Concurrent as CC
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )

import PlotTypes ( Channel(..) )
import GraphWidget ( newGraph )


animationWaitTime :: Int
animationWaitTime = 3

runPlotter :: [Channel] -> [CC.ThreadId] -> IO ()
runPlotter channels backgroundThreadsToKill = do
  _ <- Gtk.initGUI

  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "Plot-ho-matic"
                   ]

  -- on close, kill all the windows and threads
  graphWindowsToBeKilled <- CC.newMVar []
  let killEverything = do
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ CC.killThread backgroundThreadsToKill
        Gtk.mainQuit
  _ <- Gtk.onDestroy win killEverything

  --------------- main widget -----------------
  -- button to quit
  buttonQuit <- Gtk.buttonNewWithLabel "QUIT"
  _ <- Gtk.onClicked buttonQuit (Gtk.widgetDestroy win)

  -- button to clear channels
  buttonClear <- Gtk.buttonNewWithLabel "clear all values"
  _ <- Gtk.onClicked buttonClear $ putStrLn "you pressed \"clear\"" -- mapM_ clearVarInfo infos

  -- list of channels
  chanWidget <- channelWidget channels graphWindowsToBeKilled

  -- vbox to hold buttons
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox [ Gtk.containerChild := buttonQuit
               , Gtk.containerChild := buttonClear
               , Gtk.containerChild := chanWidget
               ]

  -- add widget to window and show
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI


-- the list of channels
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

  -- spawn a new graph when a checkbox is clicked
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    let (i:_) = Gtk.stringToTreePath pathStr
    channel <- Gtk.listStoreGetValue model i
    graphWin <- newGraph animationWaitTime channel
    
    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))

  return treeview
