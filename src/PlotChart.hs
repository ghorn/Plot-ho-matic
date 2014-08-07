{-# OPTIONS_GHC -Wall #-}
{-# Language PackageImports #-}

-- | One signals are selected and whatnot, this module just dumbly plots whatever
-- is in the GraphInfo data

module PlotChart
       ( AxisScaling(..)
       , GraphInfo(..)
       , XAxisType(..)
       , newChartCanvas
       ) where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Time ( NominalDiffTime )
import qualified Control.Concurrent as CC
import Control.Lens ( (.~) )
import Data.Default.Class ( def )
--import qualified Data.Foldable as F
--import qualified Data.Sequence as S
import qualified "gtk" Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Chart as Chart
import Graphics.Rendering.Chart.Backend.Cairo ( runBackend, defaultEnv )

import PlotTypes ( Getter )

-- milliseconds for draw time
animationWaitTime :: Int
animationWaitTime = 33

data XAxisType = XAxisCounter -- message number
               | XAxisShiftedCounter -- message number shifted to 0
               | XAxisTime -- message receive time
               | XAxisShiftedTime -- message receive time shifted to 0
--               | XAxisFun (String, a -> Double)

data AxisScaling = LogScaling
                 | LinearScaling

-- what the chart window needs to know to draw
data GraphInfo a =
  GraphInfo { giData :: CC.MVar (S.Seq (a, Int, NominalDiffTime))
            , giXScaling :: AxisScaling
            , giYScaling :: AxisScaling
            , giXAxisType :: XAxisType
            , giXRange :: Maybe (Double,Double)
            , giYRange :: Maybe (Double,Double)
            , giGetters :: [(String, Getter a)]
            }

newChartCanvas :: CC.MVar (GraphInfo a) -> IO Gtk.DrawingArea
newChartCanvas graphInfoMVar = do
  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  _ <- Gtk.widgetSetSizeRequest chartCanvas 250 250
  _ <- Gtk.onExpose chartCanvas $ const (updateCanvas graphInfoMVar chartCanvas)
  _ <- Gtk.timeoutAddFull
       (Gtk.widgetQueueDraw chartCanvas >> return True)
       Gtk.priorityDefaultIdle animationWaitTime
  return chartCanvas

updateCanvas :: CC.MVar (GraphInfo a) -> Gtk.DrawingArea -> IO Bool
updateCanvas graphInfoMVar canvas = do
  gi <- CC.readMVar graphInfoMVar
  datalogSeq <- CC.readMVar (giData gi)
  let datalogList = F.toList datalogSeq
      nameWithPoints :: [(String, [[(Double,Double)]])]
      nameWithPoints = map f (giGetters gi)

      (k0,t0) = case datalogList of
        [] -> (0,0)
        ((_,k0',t0'):_) -> (k0',t0')

      xAxisType = giXAxisType gi

      xaxis :: [Double]
      xaxis = case xAxisType of
        XAxisCounter        -> map (\(_,k,_) -> realToFrac  k    ) datalogList
        XAxisShiftedCounter -> map (\(_,k,_) -> realToFrac (k-k0)) datalogList
        XAxisTime           -> map (\(_,_,t) -> realToFrac t     ) datalogList
        XAxisShiftedTime    -> map (\(_,_,t) -> realToFrac (t-t0)) datalogList

      f (name, Right getter) = (name, getter datalogSeq :: [[(Double,Double)]])
      f (name, Left getter) = (name, [zip xaxis (map (\(d,_,_) -> getter d) datalogList)])

  let myGraph = displayChart xAxisType
                (giXScaling gi, giYScaling gi) (giXRange gi, giYRange gi) nameWithPoints
  chartGtkUpdateCanvas myGraph canvas

chartGtkUpdateCanvas :: Chart.Renderable a -> Gtk.DrawingArea  -> IO Bool
chartGtkUpdateCanvas chart canvas = do
    win <- Gtk.widgetGetDrawWindow canvas
    (width, height) <- Gtk.widgetGetSize canvas
    regio <- Gtk.regionRectangle $ Gtk.Rectangle 0 0 width height
    let sz = (fromIntegral width,fromIntegral height)
    Gtk.drawWindowBeginPaintRegion win regio
    _ <- Gtk.renderWithDrawable win $ runBackend (defaultEnv Chart.bitmapAlignmentFns) (Chart.render chart sz) 
    Gtk.drawWindowEndPaint win
    return True


displayChart :: (Chart.PlotValue a, Show a, RealFloat a) =>
                XAxisType ->
                (AxisScaling, AxisScaling) -> (Maybe (a,a),Maybe (a,a)) ->
                [(String, [[(a,a)]])] -> Chart.Renderable ()
displayChart xAxisType (xScaling,yScaling) (xRange,yRange) namePcs = Chart.toRenderable layout
  where
    drawOne (name,pc) col
      = Chart.plot_lines_values .~ pc
        $ Chart.plot_lines_style  . Chart.line_color .~ col
--        $ Chart.plot_points_style ~. Chart.filledCircles 2 red
        $ Chart.plot_lines_title .~ name
        $ def
    allLines = zipWith drawOne namePcs Chart.defaultColorSeq

    xscaleFun = case xScaling of
      LogScaling -> Chart.layout_x_axis . Chart.laxis_generate .~ Chart.autoScaledLogAxis def
      LinearScaling -> case xRange of
        Nothing -> id
        Just range -> Chart.layout_x_axis . Chart.laxis_generate .~ Chart.scaledAxis def range

    yscaleFun = case yScaling of
      LogScaling -> Chart.layout_y_axis . Chart.laxis_generate .~ Chart.autoScaledLogAxis def
      LinearScaling -> case yRange of
        Nothing -> id
        Just range -> Chart.layout_y_axis . Chart.laxis_generate .~ Chart.scaledAxis def range

    xlabel = case xAxisType of
      XAxisTime -> "time [s]"
      XAxisShiftedTime -> "time [s]"
      XAxisCounter -> "count"
      XAxisShiftedCounter -> "count"

    layout = Chart.layout_plots .~ map Chart.toPlot allLines
--             $ Chart.layout_title .~ "Wooo, Party Graph!"
             $ Chart.layout_x_axis . Chart.laxis_title .~ xlabel
             $ xscaleFun
             $ yscaleFun
             def
