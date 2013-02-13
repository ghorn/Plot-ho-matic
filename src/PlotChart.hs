{-# OPTIONS_GHC -Wall #-}

module PlotChart ( newChartCanvas, updateCanvas ) where

import qualified Control.Concurrent as CC
import Data.Accessor
import qualified Data.Foldable as F
import Data.Maybe ( mapMaybe )
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Chart as Chart
 
import PlotTypes ( GraphInfo(..), pbpToFrac )


newChartCanvas :: CC.MVar (GraphInfo a) -> Int -> IO Gtk.DrawingArea
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


updateCanvas :: CC.MVar (GraphInfo a) -> Gtk.DrawingArea  -> IO Bool
updateCanvas graphInfoMVar canvas = do
  (GraphInfo logSeq' numToDraw' getters) <- CC.readMVar graphInfoMVar
  numToDraw <- CC.readMVar numToDraw'
  logSeq <- CC.readMVar logSeq'
  let shortLog = S.drop (S.length logSeq - numToDraw) logSeq
      f (name,getter) = (name,fmap (pbpToFrac . getter) shortLog :: Seq (Maybe Double))
      namePcs = map f getters
  (width, height) <- Gtk.widgetGetSize canvas
  let sz = (fromIntegral width,fromIntegral height)
  win <- Gtk.widgetGetDrawWindow canvas
  _ <- Gtk.renderWithDrawable win $ Chart.runCRender (Chart.render (displayChart namePcs) sz) Chart.vectorEnv
  return True

displayChart :: (F.Foldable t, Chart.PlotValue a) => [(String, t (Maybe a))] -> Chart.Renderable ()
displayChart namePcs = Chart.toRenderable layout
  where
    f (_, Nothing) = Nothing
    f (k, Just x)  = Just (k,x)
    drawOne (name,pc) col
      = Chart.plot_lines_values ^= [mapMaybe f $ zip [(0::Int)..] (F.toList pc)]
        $ Chart.plot_lines_style  .> Chart.line_color ^= col
--        $ Chart.plot_points_style ^= Chart.filledCircles 2 red
        $ Chart.plot_lines_title ^= name
        $ Chart.defaultPlotLines
    allLines = zipWith drawOne namePcs Chart.defaultColorSeq
    layout = Chart.layout1_title ^= "Wooo"
             $ Chart.layout1_plots ^= map (Left . Chart.toPlot) allLines
             $ Chart.defaultLayout1
