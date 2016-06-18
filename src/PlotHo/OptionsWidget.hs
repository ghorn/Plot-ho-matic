{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.OptionsWidget
       ( OptionsWidget(..)
       , makeOptionsWidget
       ) where

import qualified Control.Concurrent as CC
import Control.Monad ( void )
import Data.IORef ( newIORef, readIORef, writeIORef )
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import Text.Read ( readMaybe )
import qualified Data.Text as T

import PlotHo.PlotTypes

data OptionsWidget
  = OptionsWidget
    { owVBox :: Gtk.VBox
    , owGetAxes :: IO (Axes Double)
    }

makeOptionsWidget :: CC.MVar (XY (Double, Double)) -> IO () -> IO OptionsWidget
makeOptionsWidget largestRangeMVar redraw = do
  -- user selectable range
  xRangeEntry <- Gtk.entryNew
  yRangeEntry <- Gtk.entryNew
  Gtk.set xRangeEntry
    [ Gtk.entryEditable := True
    , Gtk.widgetSensitive := True
    ]
  Gtk.set yRangeEntry
    [ Gtk.entryEditable := True
    , Gtk.widgetSensitive := True
    ]
  xRangeBox <- labeledWidget "x range:" xRangeEntry
  yRangeBox <- labeledWidget "y range:" yRangeEntry

  let updateRange rangeEntry rangeRef name = do
        txt <- Gtk.get rangeEntry Gtk.entryText
        oldRange <- readIORef rangeRef

        case readMaybe txt of
          Nothing -> do
            putStrLn $ "invalid " ++ name ++ " range entry: " ++ txt
            Gtk.set rangeEntry [Gtk.entryText := show oldRange]
          Just (z0,z1)
            | z0 >= z1 -> do
                putStrLn $ "invalid " ++ name ++ " range entry (min >= max): " ++ txt
                Gtk.set rangeEntry [Gtk.entryText := show oldRange]
            | otherwise -> do
                writeIORef rangeRef (z0, z1)
                redraw

  Gtk.set xRangeEntry [Gtk.entryText := "(-10,10)"]
  Gtk.set yRangeEntry [Gtk.entryText := "(-10,10)"]

  xRangeRef <- newIORef (-10, 10)
  yRangeRef <- newIORef (-10, 10)

  _ <- on xRangeEntry Gtk.entryActivate (updateRange xRangeEntry xRangeRef "x")
  _ <- on yRangeEntry Gtk.entryActivate (updateRange yRangeEntry yRangeRef "y")


  -- linear or log scaling on the x and y axis?
  let updateScaling scalingSelector scalingRef = do
        k <- Gtk.comboBoxGetActive scalingSelector
        _ <- case k of
          0 -> writeIORef scalingRef LinearScalingAutoRange
          1 -> writeIORef scalingRef LinearScalingHistoryRange
          2 -> writeIORef scalingRef LinearScalingManualRange
          3 -> writeIORef scalingRef LogScaling
          _ -> error "the \"impossible\" happened: scaling comboBox index should be < 4"
        redraw

  xScalingSelector <- Gtk.comboBoxNewText
  yScalingSelector <- Gtk.comboBoxNewText
  let scalingOptions =
        ["linear (auto)", "linear (history)", "linear (manual)", "logarithmic (auto)"]
  mapM_ (Gtk.comboBoxAppendText xScalingSelector . T.pack) scalingOptions
  mapM_ (Gtk.comboBoxAppendText yScalingSelector . T.pack) scalingOptions
  Gtk.comboBoxSetActive xScalingSelector 0
  Gtk.comboBoxSetActive yScalingSelector 0
  xScalingBox <- labeledWidget "x scaling:" xScalingSelector
  yScalingBox <- labeledWidget "y scaling:" yScalingSelector

  xScalingRef <- newIORef LinearScalingAutoRange
  yScalingRef <- newIORef LinearScalingAutoRange
  updateScaling xScalingSelector xScalingRef
  updateScaling yScalingSelector yScalingRef
  void $ on xScalingSelector Gtk.changed (updateScaling xScalingSelector xScalingRef)
  void $ on yScalingSelector Gtk.changed (updateScaling yScalingSelector yScalingRef)

  resetXHistoryButton <- Gtk.buttonNewWithLabel "reset X range"
  resetYHistoryButton <- Gtk.buttonNewWithLabel "reset Y range"

  void $ on resetXHistoryButton Gtk.buttonActivated $
    CC.modifyMVar_ largestRangeMVar (\xy -> return (xy {xaxis = defaultHistoryRange}))
  void $ on resetYHistoryButton Gtk.buttonActivated $
    CC.modifyMVar_ largestRangeMVar (\xy -> return (xy {yaxis = defaultHistoryRange}))

  -- vbox to hold the little window on the left
  vbox <- Gtk.vBoxNew False 4

  Gtk.set vbox
    [ Gtk.containerChild := xScalingBox
    , Gtk.boxChildPacking   xScalingBox := Gtk.PackNatural
    , Gtk.containerChild := xRangeBox
    , Gtk.boxChildPacking   xRangeBox := Gtk.PackNatural
    , Gtk.containerChild := resetXHistoryButton
    , Gtk.boxChildPacking   resetXHistoryButton := Gtk.PackNatural
    , Gtk.containerChild := yScalingBox
    , Gtk.boxChildPacking   yScalingBox := Gtk.PackNatural
    , Gtk.containerChild := yRangeBox
    , Gtk.boxChildPacking   yRangeBox := Gtk.PackNatural
    , Gtk.containerChild := resetYHistoryButton
    , Gtk.boxChildPacking   resetYHistoryButton := Gtk.PackNatural
    ]

  return
    OptionsWidget
    { owVBox = vbox
    , owGetAxes = do
        xRange <- readIORef xRangeRef
        yRange <- readIORef yRangeRef
        xScaling <- readIORef xScalingRef
        yScaling <- readIORef yScalingRef
        return
          Axes
          { axesType = XY xScaling yScaling
          , axesManualRange = XY xRange yRange
          }
    }



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
