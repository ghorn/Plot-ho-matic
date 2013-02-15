{-# OPTIONS_GHC -Wall #-}

module GraphWidget ( newGraph ) where

import qualified Control.Concurrent as CC
import Control.Monad ( when )
import Data.Maybe ( catMaybes, isJust, fromJust )
import qualified Data.Tree as Tree
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import Text.Read ( readMaybe )

import PlotTypes ( Channel(..), XAxisType(..), PbPrim )
import PlotChart ( GraphInfo(..), AxisScaling(..), newChartCanvas )

data ListViewInfo a = ListViewInfo { lviName :: String
                                   , lviGetter :: Maybe (a -> PbPrim)
                                   , lviMarked :: Bool
                                   }


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


-- make a new graph window
newGraph :: Channel -> IO Gtk.Window
newGraph (Channel {chanGetters = changetters, chanSeq = chanseq}) = do
  win <- Gtk.windowNew
  numToDrawMv <- CC.newMVar 0 -- changed immediately

  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "I am a graph"
                   ]

  -- how many to show?
  plotLength <- Gtk.entryNew
  plotLengthBox <- labeledWidget "range:" plotLength
  
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

  -- mvar with everything the graphs need to plot
  graphInfoMVar <- CC.newMVar $ GraphInfo { giData = chanseq
                                          , giLen = numToDrawMv
                                          , giXAxis = XAxisCounter
                                          , giYScaling = LinearScaling
                                          , giGetters = []
                                          }

  -- which one is the x axis?
  xaxisSelector <- Gtk.comboBoxNewText
  mapM_ (Gtk.comboBoxAppendText xaxisSelector) ["(counter)","(timestamp)"]
  let f (_,Nothing) = Nothing
      f (x,Just y) = Just (x,y)
      xaxisGetters = catMaybes $ map f (Tree.flatten changetters)
  mapM_ (Gtk.comboBoxAppendText xaxisSelector. fst) xaxisGetters
  Gtk.comboBoxSetActive xaxisSelector 0
  xaxisBox <- labeledWidget "x axis:" xaxisSelector

  let updateXAxis = do
        k <- Gtk.comboBoxGetActive xaxisSelector
        _ <- case k of
          0 -> CC.modifyMVar_ graphInfoMVar $
               \gi -> return $ gi {giXAxis = XAxisCounter}
          1 -> CC.modifyMVar_ graphInfoMVar $
               \gi -> return $ gi {giXAxis = XAxisTime}
          _ -> CC.modifyMVar_ graphInfoMVar $
               \gi -> return $ gi {giXAxis = XAxisFun (xaxisGetters !! (k-2))}
        return ()
  updateXAxis
  _ <- on xaxisSelector Gtk.changed updateXAxis


  -- linear or log scaling on the y axis?
  yscalingSelector <- Gtk.comboBoxNewText
  mapM_ (Gtk.comboBoxAppendText yscalingSelector) ["linear","logarithmic"]
  Gtk.comboBoxSetActive yscalingSelector 0
  yscalingBox <- labeledWidget "y scaling:" yscalingSelector

  let updateYScaling = do
        k <- Gtk.comboBoxGetActive yscalingSelector
        _ <- case k of
          0 -> CC.modifyMVar_ graphInfoMVar $
               \gi -> return $ gi {giYScaling = LinearScaling}
          1 -> CC.modifyMVar_ graphInfoMVar $
               \gi -> return $ gi {giYScaling = LogScaling}
          _ -> error "y scaling should be 0 or 1"
        return ()
  updateYScaling
  _ <- on yscalingSelector Gtk.changed updateYScaling


  -- create a new tree model
  let mkTreeNode (name,maybeget) = ListViewInfo name maybeget False
  model <- Gtk.treeStoreNew [fmap mkTreeNode changetters]
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
        lvis <- Gtk.treeStoreGetTree model [0]
        let newGetters = [(lviName lvi, fromJust $ lviGetter lvi) | lvi <- Tree.flatten lvis, lviMarked lvi, isJust (lviGetter lvi)]
            
        _ <- CC.modifyMVar_ graphInfoMVar (\gi0 -> return $ gi0 { giGetters = newGetters })
        return ()
  
  -- update which y axes are visible
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    -- toggle the check mark
    let treePath = Gtk.stringToTreePath pathStr
        g lvi@(ListViewInfo _ Nothing _) = putStrLn "yeah, that's not gonna work" >> return lvi
        g (ListViewInfo name maybeget marked) = return $ ListViewInfo name maybeget (not marked)
    ret <- Gtk.treeStoreChangeM model treePath g
    when (not ret) $ putStrLn "treeStoreChane fail"
    updateGraphInfo


  -- chart drawing area
  chartCanvas <- newChartCanvas graphInfoMVar

  -- vbox to hold the little window on the left
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox [ Gtk.containerChild := plotLengthBox
               , Gtk.containerChild := xaxisBox
               , Gtk.containerChild := yscalingBox
               , Gtk.containerChild := treeview
               , Gtk.boxChildPacking plotLengthBox := Gtk.PackNatural
               , Gtk.boxChildPacking xaxisBox := Gtk.PackNatural
               , Gtk.boxChildPacking yscalingBox := Gtk.PackNatural
--               , Gtk.boxChildPacking treeview := Gtk.PackNatural
               ]

  -- hbox to hold eveything
  hbox <- Gtk.hBoxNew False 4
  Gtk.set hbox [ Gtk.containerChild := vbox
               , Gtk.containerChild := chartCanvas
               , Gtk.boxChildPacking vbox := Gtk.PackNatural
               ]
  _ <- Gtk.set win [ Gtk.containerChild := hbox ]

  Gtk.widgetShowAll win
  return win
