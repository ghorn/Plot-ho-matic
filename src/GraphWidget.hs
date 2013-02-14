{-# OPTIONS_GHC -Wall #-}

module GraphWidget ( newGraph ) where

import qualified Control.Concurrent as CC
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import Text.Read ( readMaybe )

import PlotTypes ( Channel(..), XAxisType(..), PbPrim )
import PlotChart ( GraphInfo(..), updateCanvas)

data ListViewInfo a = ListViewInfo { lviName :: String
                                   , lviGetter :: a -> PbPrim
                                   , lviMarked :: Bool
                                   , lviMaxToShow :: Int
                                   }

-- make a new graph window
newGraph :: Int -> Channel -> IO Gtk.Window
newGraph animationWaitTime (Channel {chanGetters = changetters, chanSeq = chanseq}) = do
  win <- Gtk.windowNew
  numToDrawMv <- CC.newMVar 0 -- changed immediately

  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "I am a graph"
                   ]

  -- how many to show?
  plotLength <- Gtk.entryNew
  plotLengthLabel <- Gtk.labelNew (Just "range:")
  plotLengthBox <- Gtk.hBoxNew False 4
  Gtk.set plotLengthBox [ Gtk.containerChild := plotLengthLabel
                        , Gtk.containerChild := plotLength
                        , Gtk.boxChildPacking plotLengthLabel := Gtk.PackNatural
--                        , Gtk.boxChildPacking plotLength := Gtk.PackNatural
                        ]
  
  Gtk.set plotLength [Gtk.entryText := "100"]
  let updatePlotLength = do
        len <- Gtk.get plotLength Gtk.entryText
        case readMaybe len of
          Nothing -> do
            putStrLn $ "invalid non-integer range entry: " ++ len
            k' <- CC.readMVar numToDrawMv
            Gtk.set plotLength [Gtk.entryText := show k']
          Just k -> if (k < 0)
                    then do
                      putStrLn $ "invalid negative range entry: " ++ len
                      k' <- CC.readMVar numToDrawMv
                      Gtk.set plotLength [Gtk.entryText := show k']
                      return ()
                    else do
                      _ <- CC.swapMVar numToDrawMv k
                      return ()
  updatePlotLength
  _ <- on plotLength Gtk.entryActivate updatePlotLength

  -- which one is the x axis?
  xaxisSelector <- Gtk.comboBoxNewText
  mapM_ (Gtk.comboBoxAppendText xaxisSelector) ["(counter)","(timestamp)"]
  mapM_ (Gtk.comboBoxAppendText xaxisSelector . fst) changetters
  Gtk.comboBoxSetActive xaxisSelector 0

  xaxisLabel <- Gtk.labelNew (Just "x axis:")
  xaxisBox <- Gtk.hBoxNew False 4
  Gtk.set xaxisBox [ Gtk.containerChild := xaxisLabel
                   , Gtk.containerChild := xaxisSelector
                   , Gtk.boxChildPacking xaxisLabel := Gtk.PackNatural
--                   , Gtk.boxChildPacking xaxisSelector := Gtk.PackNatural
                   ]

  -- update which one is the x axis
  graphInfoMVar <- CC.newMVar (GraphInfo chanseq numToDrawMv XAxisCounter [])
  
  let updateXAxis = do
        k <- Gtk.comboBoxGetActive xaxisSelector
        _ <- case k of
          0 -> CC.modifyMVar_ graphInfoMVar $
               \(GraphInfo a b _ d) -> return (GraphInfo a b XAxisCounter d)
          1 -> CC.modifyMVar_ graphInfoMVar $
               \(GraphInfo a b _ d) -> return (GraphInfo a b XAxisTime d)
          _ -> CC.modifyMVar_ graphInfoMVar $
               \(GraphInfo a b _ d) -> return (GraphInfo a b (XAxisFun (changetters !! (k-2))) d)
        return ()
  updateXAxis
  _ <- on xaxisSelector Gtk.changed updateXAxis



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

  -- add some columns
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

  
  let -- update the graph information
      updateGraphInfo = do
        lvis <- Gtk.listStoreToList model
        let swapGraphInfo (GraphInfo _ _ xaxisget _) = GraphInfo chanseq numToDrawMv xaxisget [(lviName lvi, lviGetter lvi) | lvi <- lvis, lviMarked lvi]
            
        _ <- CC.modifyMVar_ graphInfoMVar (return . swapGraphInfo)
        return ()
  
  -- update which y axes are visible
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    -- toggle the check mark
    let (i:_) = Gtk.stringToTreePath pathStr
    lvi0 <- Gtk.listStoreGetValue model i
    Gtk.listStoreSetValue model i (lvi0 {lviMarked = not (lviMarked lvi0)})
    updateGraphInfo


  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  _ <- Gtk.widgetSetSizeRequest chartCanvas 250 250
  _ <- Gtk.onExpose chartCanvas $ const (updateCanvas graphInfoMVar chartCanvas)
  _ <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw chartCanvas
      return True)
    Gtk.priorityDefaultIdle animationWaitTime

  -- vbox to hold x axis selector and treeview
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox [ Gtk.containerChild := plotLengthBox
               , Gtk.containerChild := xaxisBox
               , Gtk.containerChild := treeview
               , Gtk.boxChildPacking plotLengthBox := Gtk.PackNatural
               , Gtk.boxChildPacking xaxisBox := Gtk.PackNatural
--               , Gtk.boxChildPacking treeview := Gtk.PackNatural
               ]

  -- hbox to hold treeview and gl drawing
  hbox <- Gtk.hBoxNew False 4
  Gtk.set hbox [ Gtk.containerChild := vbox
               , Gtk.containerChild := chartCanvas
               , Gtk.boxChildPacking vbox := Gtk.PackNatural
               ]
  _ <- Gtk.set win [ Gtk.containerChild := hbox ]

  Gtk.widgetShowAll win
  return win
