{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlotHo.ChartRender
       ( AxisScaling(..)
       , toChartRender
       ) where

import Control.Lens ( (.~) )
import Control.Monad ( void )
import Data.Default.Class ( def )
import qualified Graphics.Rendering.Chart as Chart
import Graphics.Rendering.Chart.Backend.Cairo ( runBackend, defaultEnv )
import qualified Graphics.Rendering.Cairo as Cairo

import PlotHo.PlotTypes ( AxisScaling(..) )

-- take the data and use Chart to make a Renderable ()
toChartRender :: forall a
                 . (Chart.PlotValue a, Show a, RealFloat a)
                 => (AxisScaling, AxisScaling) -> (Maybe (a,a), Maybe (a,a))
                 -> Maybe String
                 -> [(String, [[(a,a)]])]
                 -> Chart.RectSize
                 -> Cairo.Render ()
toChartRender (xScaling, yScaling) (xRange, yRange) mtitle namePcs rectSize =
  void $ runBackend (defaultEnv Chart.bitmapAlignmentFns) (Chart.render renderable rectSize)
  where
    renderable :: Chart.Renderable ()
    renderable = Chart.toRenderable layout

    drawOne (name,pc) col
      = Chart.plot_lines_values .~ pc
        $ Chart.plot_lines_style  . Chart.line_color .~ col
--        $ Chart.plot_points_style ~. Chart.filledCircles 2 red
        $ Chart.plot_lines_title .~ name
        $ def
    allLines :: [Chart.PlotLines a a]
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

    title = case mtitle of
      Nothing -> id
      Just t -> Chart.layout_title .~ t
    layout = Chart.layout_plots .~ map Chart.toPlot allLines
             $ title
             $ Chart.layout_x_axis . Chart.laxis_title .~ "time [s]"
             $ xscaleFun
             $ yscaleFun
             def
