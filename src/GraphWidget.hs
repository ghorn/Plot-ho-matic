{-# OPTIONS_GHC -Wall #-}

module GraphWidget ( newGraph ) where

import qualified Control.Concurrent as CC
import Control.Monad ( unless )
import Data.Maybe ( mapMaybe, isJust, fromJust )
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

  -- mvar with everything the graphs need to plot
  graphInfoMVar <- CC.newMVar GraphInfo { giData = chanseq
                                        , giLen = 0 -- changed immediately
                                        , giXAxis = XAxisCounter
                                        , giXScaling = LinearScaling
                                        , giYScaling = LinearScaling
                                        , giXRange = Nothing
                                        , giYRange = Nothing
                                        , giGetters = []
                                        }

  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "I am a graph"
                   ]

  -- how many to show?
  plotLength <- Gtk.entryNew
  plotLengthBox <- labeledWidget "# points to plot:" plotLength
  
  Gtk.set plotLength [Gtk.entryText := "100"]
  let updatePlotLength = do
        txt <- Gtk.get plotLength Gtk.entryText
        gi <- CC.readMVar graphInfoMVar
        case readMaybe txt of
          Nothing -> do
            putStrLn $ "invalid non-integer range entry: " ++ txt
            Gtk.set plotLength [Gtk.entryText := show (giLen gi)]
          Just k -> if k < 0
                    then do
                      putStrLn $ "invalid negative range entry: " ++ txt
                      Gtk.set plotLength [Gtk.entryText := show (giLen gi)]
                      return ()
                    else do
                      _ <- CC.swapMVar graphInfoMVar (gi {giLen = k})
                      return ()
  updatePlotLength
  _ <- on plotLength Gtk.entryActivate updatePlotLength

  -- which one is the x axis?
  xaxisSelector <- Gtk.comboBoxNewText
  mapM_ (Gtk.comboBoxAppendText xaxisSelector) ["(counter)","(timestamp)"]
  let f (_,Nothing) = Nothing
      f (x,Just y) = Just (x,y)
      xaxisGetters = mapMaybe f (Tree.flatten changetters)
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


  -- user selectable range
  xRange <- Gtk.entryNew
  yRange <- Gtk.entryNew
  Gtk.set xRange [ Gtk.entryEditable := False
                 , Gtk.widgetSensitive := False
                 ]
  Gtk.set yRange [ Gtk.entryEditable := False
                 , Gtk.widgetSensitive := False
                 ]
  xRangeBox <- labeledWidget "x range:" xRange
  yRangeBox <- labeledWidget "y range:" yRange
  Gtk.set xRange [Gtk.entryText := "(-10,10)"]
  Gtk.set yRange [Gtk.entryText := "(-10,10)"]
  let updateXRange = do
        Gtk.set xRange [ Gtk.entryEditable := True
                       , Gtk.widgetSensitive := True
                       ]
        txt <- Gtk.get xRange Gtk.entryText
        gi <- CC.readMVar graphInfoMVar
        case readMaybe txt of
          Nothing -> do
            putStrLn $ "invalid x range entry: " ++ txt
            Gtk.set xRange [Gtk.entryText := "(min,max)"]
          Just (z0,z1) -> if z0 >= z1
                    then do
                      putStrLn $ "invalid x range entry (min >= max): " ++ txt
                      Gtk.set xRange [Gtk.entryText := "(min,max)"]
                      return ()
                    else do
                      _ <- CC.swapMVar graphInfoMVar (gi {giXRange = Just (z0,z1)})
                      return ()
  let updateYRange = do
        Gtk.set yRange [ Gtk.entryEditable := True
                       , Gtk.widgetSensitive := True
                       ]
        txt <- Gtk.get yRange Gtk.entryText
        gi <- CC.readMVar graphInfoMVar
        case readMaybe txt of
          Nothing -> do
            putStrLn $ "invalid y range entry: " ++ txt
            Gtk.set yRange [Gtk.entryText := "(min,max)"]
          Just (z0,z1) -> if z0 >= z1
                    then do
                      putStrLn $ "invalid y range entry (min >= max): " ++ txt
                      Gtk.set yRange [Gtk.entryText := "(min,max)"]
                      return ()
                    else do
                      _ <- CC.swapMVar graphInfoMVar (gi {giYRange = Just (z0,z1)})
                      return ()
  _ <- on xRange Gtk.entryActivate updateXRange
  _ <- on yRange Gtk.entryActivate updateYRange

  -- linear or log scaling on the x and y axis?
  xScalingSelector <- Gtk.comboBoxNewText
  yScalingSelector <- Gtk.comboBoxNewText
  mapM_ (Gtk.comboBoxAppendText xScalingSelector)
    ["linear (auto)","linear (manual)","logarithmic (auto)"]
  mapM_ (Gtk.comboBoxAppendText yScalingSelector)
    ["linear (auto)","linear (manual)","logarithmic (auto)"]
  Gtk.comboBoxSetActive xScalingSelector 0
  Gtk.comboBoxSetActive yScalingSelector 0
  xScalingBox <- labeledWidget "x scaling:" xScalingSelector
  yScalingBox <- labeledWidget "y scaling:" yScalingSelector
  let updateXScaling = do
        k <- Gtk.comboBoxGetActive xScalingSelector
        _ <- case k of
          0 -> do
            Gtk.set xRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = LinearScaling, giXRange = Nothing}
          1 -> do
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = LinearScaling, giXRange = Nothing}
            updateXRange
          2 -> do
            Gtk.set xRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = LogScaling, giXRange = Nothing}
          _ -> error "the \"impossible\" happened: x scaling comboBox index should be < 3"
        return ()
  let updateYScaling = do
        k <- Gtk.comboBoxGetActive yScalingSelector
        _ <- case k of
          0 -> do
            Gtk.set yRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giYScaling = LinearScaling, giYRange = Nothing}
          1 -> do
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giYScaling = LinearScaling, giYRange = Nothing}
            updateYRange
          2 -> do
            Gtk.set yRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giYScaling = LogScaling, giYRange = Nothing}
          _ -> error "the \"impossible\" happened: y scaling comboBox index should be < 3"
        return ()
  updateXScaling
  updateYScaling
  _ <- on xScalingSelector Gtk.changed updateXScaling
  _ <- on yScalingSelector Gtk.changed updateYScaling

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
    unless ret $ putStrLn "treeStoreChange fail"
    updateGraphInfo


  -- chart drawing area
  chartCanvas <- newChartCanvas graphInfoMVar

  -- vbox to hold the little window on the left
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox [ Gtk.containerChild := plotLengthBox
               , Gtk.boxChildPacking   plotLengthBox := Gtk.PackNatural
               , Gtk.containerChild := xaxisBox
               , Gtk.boxChildPacking   xaxisBox := Gtk.PackNatural
               , Gtk.containerChild := xScalingBox
               , Gtk.boxChildPacking   xScalingBox := Gtk.PackNatural
               , Gtk.containerChild := xRangeBox
               , Gtk.boxChildPacking   xRangeBox := Gtk.PackNatural
               , Gtk.containerChild := yScalingBox
               , Gtk.boxChildPacking   yScalingBox := Gtk.PackNatural
               , Gtk.containerChild := yRangeBox
               , Gtk.boxChildPacking   yRangeBox := Gtk.PackNatural
               , Gtk.containerChild := treeview
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
