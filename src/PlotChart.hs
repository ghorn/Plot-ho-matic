{-# OPTIONS_GHC -Wall #-}

module PlotChart ( AxisScaling(..), GraphInfo(..), newChartCanvas, updateCanvas ) where

import qualified Control.Concurrent as CC
import Data.Accessor
import qualified Data.Foldable as F
import Data.Maybe ( mapMaybe )
import Data.Sequence ( Seq, ViewR(..) )
import qualified Data.Sequence as S
import Data.Time ( NominalDiffTime )
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Chart as Chart
 
import PlotTypes ( XAxisType(..), PbPrim(..) )

data AxisScaling = LogScaling
                 | LinearScaling

-- what the graph should draw
data GraphInfo a = GraphInfo { giData :: CC.MVar (S.Seq (a,Int,NominalDiffTime))
                             , giLen :: Int
                             , giXAxis :: XAxisType a
                             , giXScaling :: AxisScaling
                             , giYScaling :: AxisScaling
                             , giXRange :: Maybe (Double,Double)
                             , giYRange :: Maybe (Double,Double)
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

pbpToFrac :: Fractional a => PbPrim -> Maybe a
pbpToFrac (PbDouble c)     = Just $ realToFrac c
pbpToFrac (PbFloat c)      = Just $ realToFrac c
pbpToFrac (PbInt32 c)      = Just $ realToFrac c
pbpToFrac (PbInt64 c)      = Just $ realToFrac c
pbpToFrac (PbWord32 c)     = Just $ realToFrac c
pbpToFrac (PbWord64 c)     = Just $ realToFrac c
pbpToFrac (PbBool c)       = Just $ (\x -> if x then 1 else 0) c
pbpToFrac (PbUtf8 _)       = Nothing
pbpToFrac (PbByteString _) = Nothing
pbpToFrac (PbSeq _) = Nothing
pbpToFrac (PbMaybe _) = Nothing

updateCanvas :: Gtk.WidgetClass widget => CC.MVar (GraphInfo a) -> widget -> IO Bool
updateCanvas graphInfoMVar canvas = do
  gi <- CC.readMVar graphInfoMVar
  datalog <- CC.readMVar (giData gi)
  let shortLog = S.drop (S.length datalog - giLen gi) datalog
      f (name,getter) = (name,fmap (\(x,_,_) -> (getter x)) shortLog :: Seq PbPrim)

      namePcs' :: [(String, Seq PbPrim)]
      namePcs' = map f (giGetters gi)

      g :: (String,Seq PbPrim) -> (String, Seq (Maybe Double))
      g (name,xs) = case S.viewr xs of
        EmptyR -> (name, S.empty)
        _ :> (PbSeq xs') -> (name, fmap pbpToFrac xs')
        _ -> (name, fmap pbpToFrac xs)
      namePcs = map g namePcs'

      (xaxisName, xaxisVals) = case giXAxis gi of
        XAxisCounter -> ("count", fmap (\(_,k,_) -> Just (fromIntegral k)) shortLog)
        XAxisStaticCounter -> ("static count", fmap Just $ S.fromList $ take (S.length shortLog) [0..])
        XAxisTime -> ("msg receive timestamp [s]", fmap (\(_,_,t) -> Just (realToFrac t)) shortLog)
        XAxisFun (name,getx) -> (name, fmap (\(x,_,_) -> pbpToFrac (getx x)) shortLog)
  (width, height) <- Gtk.widgetGetSize canvas
  let sz = (fromIntegral width,fromIntegral height)
  win <- Gtk.widgetGetDrawWindow canvas
  let myGraph = displayChart (giXScaling gi, giYScaling gi) (giXRange gi, giYRange gi)
                xaxisName xaxisVals namePcs
  _ <- Gtk.renderWithDrawable win $ Chart.runCRender (Chart.render myGraph sz) Chart.vectorEnv
  return True

displayChart :: (Chart.PlotValue a, Show a, RealFloat a) =>
                (AxisScaling, AxisScaling) -> (Maybe (a,a),Maybe (a,a)) -> String ->
                Seq (Maybe a) -> [(String, Seq (Maybe a))] -> Chart.Renderable ()
displayChart (xScaling,yScaling) (xRange,yRange) xaxisName xaxis namePcs = Chart.toRenderable layout
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

    xscaleFun = case xScaling of
      LogScaling -> Chart.layout1_bottom_axis .> Chart.laxis_generate ^= Chart.autoScaledLogAxis Chart.defaultLogAxis
      LinearScaling -> case xRange of
        Nothing -> id
        Just range -> Chart.layout1_bottom_axis .> Chart.laxis_generate ^= Chart.scaledAxis Chart.defaultLinearAxis range

    yscaleFun = case yScaling of
      LogScaling -> Chart.layout1_left_axis .> Chart.laxis_generate ^= Chart.autoScaledLogAxis Chart.defaultLogAxis
      LinearScaling -> case yRange of
        Nothing -> id
        Just range -> Chart.layout1_left_axis .> Chart.laxis_generate ^= Chart.scaledAxis Chart.defaultLinearAxis range

    layout = Chart.layout1_title ^= "Wooo"
             $ Chart.layout1_plots ^= map (Left . Chart.toPlot) allLines
             $ Chart.layout1_bottom_axis .> Chart.laxis_title ^= xaxisName
             $ xscaleFun
             $ yscaleFun
             $ Chart.defaultLayout1
