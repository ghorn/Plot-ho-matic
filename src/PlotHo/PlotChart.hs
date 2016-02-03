{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.PlotChart
       ( AxisScaling(..)
       , displayChart
       , chartGtkUpdateCanvas
       ) where

import Control.Lens ( (.~) )
import Data.Default.Class ( def )
--import qualified Data.Foldable as F
--import qualified Data.Sequence as S
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Chart as Chart
import Graphics.Rendering.Chart.Backend.Cairo ( runBackend, defaultEnv )
import Graphics.Rendering.Cairo
       ( Render, Format(..)
       , renderWith, withImageSurface, setSourceSurface, paint
       )

import PlotHo.PlotTypes ( AxisScaling(..) )

chartGtkUpdateCanvas :: Chart.Renderable () -> Gtk.DrawingArea  -> IO ()
chartGtkUpdateCanvas chart canvas = do
    Gtk.threadsEnter
    maybeWin <- Gtk.widgetGetWindow canvas
    case maybeWin of
      Nothing -> Gtk.threadsLeave >> return ()
      Just win -> do
        Gtk.Rectangle _ _ width height <- Gtk.widgetGetAllocation canvas
        Gtk.threadsLeave
        let sz = (fromIntegral width,fromIntegral height)
        let render0 :: Render (Chart.PickFn ())
            render0 = runBackend (defaultEnv Chart.bitmapAlignmentFns) (Chart.render chart sz)

        withImageSurface FormatARGB32 width height $ \surface -> do
          _ <- renderWith surface render0
          let render1 = setSourceSurface surface 0 0 >> paint
          Gtk.threadsEnter
          Gtk.drawWindowBeginPaintRect win (Gtk.Rectangle 0 0 width height)
          _ <- Gtk.renderWithDrawWindow win render1
          Gtk.drawWindowEndPaint win
          Gtk.threadsLeave

displayChart :: forall a
                . (Chart.PlotValue a, Show a, RealFloat a)
                => (AxisScaling, AxisScaling) -> (Maybe (a,a), Maybe (a,a))
                -> Maybe String
                -> [(String, [[(a,a)]])] -> Chart.Renderable ()
displayChart (xScaling, yScaling) (xRange, yRange) mtitle namePcs = Chart.toRenderable layout
  where
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
