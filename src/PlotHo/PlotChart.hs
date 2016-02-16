{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.PlotChart
       ( AxisScaling(..)
       , displayChart
       , chartGtkUpdateCanvas
       , chartGtkUpdateCanvas'
       ) where

import Control.Lens ( (.~) )
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Default.Class ( def )
--import qualified Data.Foldable as F
--import qualified Data.Sequence as S
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Chart as Chart
import Graphics.Rendering.Chart.Backend.Cairo ( runBackend, defaultEnv )
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo
       ( Render, Format(..)
       , renderWith, withImageSurface, setSourceSurface, paint
       )

import PlotHo.PlotTypes ( AxisScaling(..) )

-- take a Chart render and draw it on a GTK canvas
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

      -- initialize a surface
      withImageSurface FormatARGB32 width height $ \surface -> do
        -- render onto that surface
        _ <- Cairo.renderWith surface render0

        -- now inside the GTK thread, paint the gtk canvas by copying from
        -- what we just did in another thread
        Gtk.threadsEnter
        let render1 :: Render ()
            render1 = setSourceSurface surface 0 0 >> paint
        Gtk.drawWindowBeginPaintRect win (Gtk.Rectangle 0 0 width height)
        _ <- Gtk.renderWithDrawWindow win render1
        Gtk.drawWindowEndPaint win
        Gtk.threadsLeave

-- take a Chart render and draw it on a GTK canvas
chartGtkUpdateCanvas' :: Gtk.Rectangle
                         -> (Chart.RectSize -> Cairo.Render ())
                         -> Cairo.Surface
                         -> Render ()
chartGtkUpdateCanvas' allocation render targetSurface = do
  matrix <- Cairo.getMatrix
  (_, _, width, height) <- Cairo.clipExtents
  liftIO $ putStrLn $ "allocation: " ++ show allocation
  liftIO $ putStrLn $ "matrix : " ++ show matrix

--  Cairo.renderWith targetSurface $ do
--    Cairo.setMatrix matrix
--    render (width, height)

  similarSurface <- liftIO $ Cairo.createSimilarSurface targetSurface Cairo.ContentColorAlpha
                    (round width) (round height)
  -- render onto that surface
  Cairo.renderWith similarSurface (render (width, height))

    -- copy onto targetSurface
  Cairo.setSourceSurface similarSurface 0 0
  paint



-- take the data and use Chart to make a Renderable ()
displayChart :: forall a
                . (Chart.PlotValue a, Show a, RealFloat a)
                => (AxisScaling, AxisScaling) -> (Maybe (a,a), Maybe (a,a))
                -> Maybe String
                -> [(String, [[(a,a)]])]
                -> Chart.RectSize
                -> Render ()
displayChart (xScaling, yScaling) (xRange, yRange) mtitle namePcs rectSize =
  void $ runBackend (defaultEnv Chart.bitmapAlignmentFns) (Chart.render renderable rectSize)
  --(Chart.render renderable rectSize)
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
