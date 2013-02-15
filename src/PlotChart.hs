{-# OPTIONS_GHC -Wall #-}

module PlotChart ( AxisScaling(..), GraphInfo(..), newChartCanvas, updateCanvas ) where

import qualified Control.Concurrent as CC
import Data.Accessor
import qualified Data.Foldable as F
import Data.Maybe ( mapMaybe )
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import Data.Time ( NominalDiffTime )
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Chart as Chart
 
import PlotTypes ( XAxisType(..), PbPrim, pbpToFrac )

data AxisScaling = LogScaling
                 | LinearScaling

-- what the graph should draw
data GraphInfo a = GraphInfo { giData :: (CC.MVar (S.Seq (a,Int,NominalDiffTime)))
                             , giLen :: CC.MVar Int
                             , giXAxis :: XAxisType a
                             , giYScaling :: AxisScaling
                             , giGetters :: [(String, a -> PbPrim)]
                             }

-- milliseconds for draw time
animationWaitTime :: Int
animationWaitTime = 33

newChartCanvas :: CC.MVar (GraphInfo a) -> IO Gtk.DrawingArea
newChartCanvas graphInfoMVar = do
  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  _ <- Gtk.widgetSetSizeRequest chartCanvas 250 250
  _ <- Gtk.onExpose chartCanvas $ const (updateCanvas graphInfoMVar chartCanvas)
  _ <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw chartCanvas
      return True)
    Gtk.priorityDefaultIdle animationWaitTime
  return chartCanvas


updateCanvas :: CC.MVar (GraphInfo a) -> Gtk.DrawingArea  -> IO Bool
updateCanvas graphInfoMVar canvas = do
  (GraphInfo logSeq' numToDraw' xaxis yScaling getters) <- CC.readMVar graphInfoMVar
  numToDraw <- CC.readMVar numToDraw'
  logSeq <- CC.readMVar logSeq'
  let shortLog = S.drop (S.length logSeq - numToDraw) logSeq
      f (name,getter) = (name,fmap (\(x,_,_) -> pbpToFrac (getter x)) shortLog :: Seq (Maybe Double))
      namePcs = map f getters

      (xaxisName, xaxisVals) = case xaxis of
        XAxisCounter -> ("count", fmap (\(_,k,_) -> Just (fromIntegral k)) shortLog)
        XAxisTime -> ("msg receive timestamp [s]", fmap (\(_,_,t) -> Just (realToFrac t)) shortLog)
        XAxisFun (name,getx) -> (name, fmap (\(x,_,_) -> pbpToFrac (getx x)) shortLog)
  (width, height) <- Gtk.widgetGetSize canvas
  let sz = (fromIntegral width,fromIntegral height)
  win <- Gtk.widgetGetDrawWindow canvas
  _ <- Gtk.renderWithDrawable win $ Chart.runCRender (Chart.render (displayChart yScaling xaxisName xaxisVals namePcs) sz) Chart.vectorEnv
  return True

displayChart :: (F.Foldable t, Chart.PlotValue a, Show a, RealFloat a) =>
                AxisScaling -> String -> t (Maybe a) -> [(String, t (Maybe a))] -> Chart.Renderable ()
displayChart yScaling xaxisName xaxis namePcs = Chart.toRenderable layout
  where
    f (Just x, Just y)  = Just (x,y)
    f _ = Nothing
    drawOne (name,pc) col
      = Chart.plot_lines_values ^= [mapMaybe f $ zip (F.toList xaxis) (F.toList pc)]
        $ Chart.plot_lines_style  .> Chart.line_color ^= col
--        $ Chart.plot_points_style ^= Chart.filledCircles 2 red
        $ Chart.plot_lines_title ^= name
        $ Chart.defaultPlotLines
    allLines = zipWith drawOne namePcs Chart.defaultColorSeq

    yscaleFun = case yScaling of
      LogScaling -> Chart.layout1_left_axis .> Chart.laxis_generate ^= Chart.autoScaledLogAxis Chart.defaultLogAxis
      LinearScaling -> id

    layout = Chart.layout1_title ^= "Wooo"
             $ Chart.layout1_plots ^= map (Left . Chart.toPlot) allLines
             $ Chart.layout1_bottom_axis .> Chart.laxis_title ^= xaxisName
             $ yscaleFun
             $ Chart.defaultLayout1
