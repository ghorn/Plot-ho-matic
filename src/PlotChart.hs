{-# OPTIONS_GHC -Wall #-}

module PlotChart ( newChartCanvas, updateCanvas ) where

import qualified Control.Concurrent as C
import Data.Accessor
import qualified Data.Foldable as F
import Data.Sequence ( Seq )
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Chart as Chart
 
import PlotTypes ( GraphInfo(..), toFrac )


newChartCanvas :: C.MVar GraphInfo -> Int -> IO Gtk.DrawingArea
newChartCanvas graphInfoMVar animationWaitTime = do
  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  _ <- Gtk.widgetSetSizeRequest chartCanvas 250 250
  _ <- Gtk.onExpose chartCanvas $ const (updateCanvas graphInfoMVar chartCanvas)
  _ <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw chartCanvas
      return True)
    Gtk.priorityDefaultIdle animationWaitTime
  return chartCanvas


updateCanvas :: C.MVar GraphInfo -> Gtk.DrawingArea  -> IO Bool
updateCanvas graphInfoMVar canvas = do
  (GraphInfo gis) <- C.readMVar graphInfoMVar
  let f (name,mv) = do
        pc <- C.readMVar mv
        return (name,toFrac pc :: Seq Double)
  namePcs <- mapM f gis
  (width, height) <- Gtk.widgetGetSize canvas
  let sz = (fromIntegral width,fromIntegral height)
  win <- Gtk.widgetGetDrawWindow canvas
  _ <- Gtk.renderWithDrawable win $ Chart.runCRender (Chart.render (displayChart namePcs) sz) Chart.vectorEnv
  return True

displayChart :: (F.Foldable t, Chart.PlotValue y0) => [(String, t y0)] -> Chart.Renderable ()
displayChart namePcs = Chart.toRenderable layout
  where
    drawOne (name,pc) col
      = Chart.plot_lines_values ^= [zip [(0::Int)..] (F.toList pc)]
        $ Chart.plot_lines_style  .> Chart.line_color ^= col
--        $ Chart.plot_points_style ^= Chart.filledCircles 2 red
        $ Chart.plot_lines_title ^= name
        $ Chart.defaultPlotLines
    allLines = zipWith drawOne namePcs Chart.defaultColorSeq
    layout = Chart.layout1_title ^= "Wooo"
             $ Chart.layout1_plots ^= map (Left . Chart.toPlot) allLines
             $ Chart.defaultLayout1
