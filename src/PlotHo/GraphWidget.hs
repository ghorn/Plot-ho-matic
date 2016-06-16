{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.GraphWidget
       ( newGraph
       ) where

import Control.Concurrent ( MVar )
import qualified Control.Concurrent as CC
import Control.Monad ( forever, void, when )
import Control.Monad.IO.Class ( liftIO )
import Data.List ( foldl' )
import qualified Data.Map.Strict as M
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import Graphics.Rendering.Cairo ( Render, Surface )
import qualified Graphics.Rendering.Cairo as Cairo
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import Text.Printf ( printf )
import Graphics.Rendering.Chart ( RectSize )

import PlotHo.ChartRender ( toChartRender )
import PlotHo.OptionsWidget ( OptionsWidget(..), makeOptionsWidget )
import PlotHo.PlotTypes
import PlotHo.SignalSelector ( SignalSelector(..), newSignalSelectorArea )

-- make a new graph window
newGraph ::
  PlotterOptions
  -> Channel -> IO Gtk.Window
newGraph opts
  (Channel
   (Channel'
    { chanName = name
    , chanLatestValueMVar = latestValueMVar
    , chanGraphCommsMap = graphCommsMap
    })) = newGraph' opts name (CC.readMVar latestValueMVar) graphCommsMap


-- make a new graph window
newGraph' ::
  forall a
  . PlotterOptions
  -> String
  -> IO (Maybe (a, SignalTree a))
  -> CC.MVar (M.Map Gtk.Window (GraphComms a))
  -> IO Gtk.Window
newGraph' options channame readLatestChannelValue graphCommsMap = do
  win <- Gtk.windowNew

  void $ Gtk.set win
    [ Gtk.containerBorderWidth := 8
    , Gtk.windowTitle := channame
    ]

  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  void $ Gtk.widgetSetSizeRequest chartCanvas 80 80

  -- mvars for drawing thread inputs/outputs
  latestOneToRenderMVar <- CC.newEmptyMVar :: IO (MVar (RectSize -> Render (), (Int, Int)))
  latestSurfaceMVar <- CC.newMVar Nothing :: IO (MVar (Maybe (Surface, (Int, Int))))

  -- fork the thread which continuously draws
  void $ CC.forkIO (renderWorker latestOneToRenderMVar latestSurfaceMVar chartCanvas options)

  -- Flag which marks if someone has called for a redraw.
  -- We have the MVar in addition to the GTK signal so that if multiple sources
  -- request a redraw and multiple signals are in the queue, we can draw once and then
  -- ignore the rest of the signals..
  needRedrawMVar <- CC.newMVar False
  let redraw :: IO ()
      redraw = do
        debug "redraw called"
        void $ CC.swapMVar needRedrawMVar True
        Gtk.postGUIAsync (Gtk.widgetQueueDraw chartCanvas)

  signalSelector <- newSignalSelectorArea redraw

  largestRangeMVar <- CC.newMVar (XY defaultHistoryRange defaultHistoryRange)
  optionsWidget <- makeOptionsWidget largestRangeMVar redraw

  msgStore <- (CC.newMVar =<<) $ do
    latestChannelValue <- readLatestChannelValue
    return $ case latestChannelValue of
      Nothing -> Nothing
      Just (val, signalTree) -> Just (val, Just signalTree)

  let handleDraw :: Render ()
      handleDraw = do
        debug "handleDraw: called"

        -- get the size of the surface we have to draw
        Gtk.Rectangle _ _ width height <- liftIO $ Gtk.widgetGetAllocation chartCanvas

        -- handleDraw always immediately takes the last rendered surface and draws it
        -- this is just a copy and very efficient
        maybeLatestSurface <- liftIO $ CC.readMVar latestSurfaceMVar
        needFirstDrawOrResizeDraw <- case maybeLatestSurface of
          Just (latestSurface, (lastWidth, lastHeight)) -> do
            -- TODO(greg): Should we be drawing if the width/height don't match?
            -- I wonder if this could cause a buffer overrun.
            debug "handleDraw: painting latest surface"
            Cairo.setSourceSurface latestSurface 0 0
            Cairo.paint
            return ((lastWidth, lastHeight) /= (width, height))
          Nothing -> do
            debug "handleDraw: no surface yet"
            return True

        -- then we determine if we need to re-generate a new surface
        needRedraw <- liftIO $ CC.swapMVar needRedrawMVar False
        when (needRedraw || needFirstDrawOrResizeDraw) $ liftIO $ do
           -- if we need to redraw for whatever reason
          case (needRedraw, needFirstDrawOrResizeDraw) of
            (True, True) -> debug $ "handleDraw: putting a redraw in because " ++
                            "needRedraw && needFirstDrawOrResizeDraw"
            (True, False) -> debug $ "handleDraw: putting a redraw in because " ++
                             "needRedraw"
            (False, True) -> debug $ "handleDraw: putting a redraw in because " ++
                             "needFirstDrawOrResizeDraw"
            _ -> return () -- (impossible)

          -- get the latest plot points
          (mtitle, namedPlotPoints) <- do
            -- get the latest data, just block if they're not available
            mdatalog <- CC.takeMVar msgStore
            case mdatalog of
              -- no data yet, return [] but do the loop as usual
              Nothing -> do
                CC.putMVar msgStore mdatalog
                return (Nothing, [])
              Just (datalog, msignalTree) -> do
                -- If there is a new signal tree, we have to merge it with the old one.
                case msignalTree of
                  Just newSignalTree -> ssRebuildSignalTree signalSelector newSignalTree
                  Nothing -> return ()

                -- Put the data back. Put Nothing to signify that the signal tree is up to date.
                CC.putMVar msgStore (Just (datalog, Nothing))

                -- Rebuilding the signal tree if necessary has now made the getters
                -- safe to apply to the latest data.
                gi <- ssReadGraphInfo signalSelector

                return (giTitle gi, map (fmap (\g -> g datalog)) (giGetters gi))

          debug "handleDraw: got title and plot points"
          let -- update the min/max plot ranges
              updateRanges :: XY (Double, Double) -> XY (Double, Double)
              updateRanges oldRanges =
                foldl' largestRange oldRanges (concatMap (concat . snd) namedPlotPoints)
          newRanges <- modifyMVar' largestRangeMVar updateRanges

          axes <- owGetAxes optionsWidget

          -- prepare the next render
          let render :: RectSize -> Render ()
              render = toChartRender axes newRanges mtitle namedPlotPoints

          -- Empty the mvar if it is full.
          -- If we are getting lots of messages quickly this
          -- will descard any undrawn requests.
          void $ CC.tryTakeMVar latestOneToRenderMVar

          -- Put the latest request in the draw thread's queue
          -- The MVar is now definitely empty so we will never block
          -- by putting something in it.
          CC.putMVar latestOneToRenderMVar (render, (width, height))

  -- connect the draw signal to our draw handler
  void $ on chartCanvas Gtk.draw handleDraw

  -- the options widget
  optionsExpander <- Gtk.expanderNew "opt"
  Gtk.set optionsExpander
    [ Gtk.containerChild := owVBox optionsWidget
    , Gtk.expanderExpanded := False
    ]

  -- the signal selector
  treeviewScroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.set treeviewScroll [Gtk.widgetVExpand := True] -- make sure it expands vertically
  Gtk.containerAdd treeviewScroll (ssTreeView signalSelector)
  Gtk.set treeviewScroll
    [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
    , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
    ]

  treeviewExpander <- Gtk.expanderNew "sig"
  Gtk.set treeviewExpander
    [ Gtk.containerChild := treeviewScroll
    , Gtk.expanderExpanded := True
    ]

  -- options and signal selector packed in vbox
  vboxOptionsAndSignals <- Gtk.vBoxNew False 4
  Gtk.set vboxOptionsAndSignals
    [ Gtk.containerChild := optionsExpander
    , Gtk.boxChildPacking optionsExpander := Gtk.PackNatural
    , Gtk.containerChild := treeviewExpander
    , Gtk.boxChildPacking treeviewExpander := Gtk.PackGrow
    ]

  -- hbox to hold eveything
  hboxEverything <- Gtk.hBoxNew False 4
  Gtk.set hboxEverything
    [ Gtk.containerChild := vboxOptionsAndSignals
    , Gtk.boxChildPacking vboxOptionsAndSignals := Gtk.PackNatural
    , Gtk.containerChild := chartCanvas
    , Gtk.boxChildPacking chartCanvas := Gtk.PackGrow
    ]
  void $ Gtk.set win
    [ Gtk.containerChild := hboxEverything ]

  -- add this window to the set of windows that get redraw signals on new messages
  let graphComms =
        GraphComms
        { gcRedrawSignal = redraw
        , gcMsgStore = msgStore
        }
  CC.modifyMVar_ graphCommsMap (return . M.insert win graphComms)

  -- when the window is closed, remove it from the set which get redraw signals on new messages
  void $ on win Gtk.deleteEvent $ do
    debug "removing window from redrawSignalMap"
    liftIO $ CC.modifyMVar_ graphCommsMap (return . M.delete win)
    return False

  -- show the window and return
  Gtk.widgetShowAll win
  return win



renderWorker
  :: MVar (RectSize -> Render (), (Int, Int))
     -> MVar (Maybe (Surface, (Int, Int)))
     -> Gtk.DrawingArea
     -> PlotterOptions
     -> IO ()
renderWorker latestOneToRenderMVar latestSurfaceMVar chartCanvas options = forever $ do
  debug "renderWorker: waiting for new render"
  -- block until we have to render something
  (render,  (width, height)) <- CC.takeMVar latestOneToRenderMVar
  renderStartTime <- getCurrentTime
  debug "renderWorker: starting render"

  -- create an image to draw on
  surface <- liftIO $ Cairo.createImageSurface Cairo.FormatARGB32 width height

  -- do the drawing
  Cairo.renderWith surface (render (realToFrac width, realToFrac height))

  -- put our new drawing in the latest surface variable
  debug "renderWorker: putting finished surface"
  void $ CC.swapMVar latestSurfaceMVar (Just (surface, (width, height)))

  -- queue another draw
  debug "renderWorker: queuing draw"
  Gtk.postGUIAsync (Gtk.widgetQueueDraw chartCanvas)

  -- At this point the render worker would immediately start the next render if one was available.
  -- This could cause us to draw at an unneccesarily high rate which would could
  -- overload the system. So we only draw at maximum rate given by 'maxDrawRate'.
  -- If we are already slower than 'maxDrawRate' we don't sleep,
  -- we just update as quickly as possible.
  renderFinishTime <- getCurrentTime
  let renderTime :: Double
      renderTime = realToFrac $ diffUTCTime renderFinishTime renderStartTime

      sleepTime = 1 / maxDrawRate options - renderTime

  debug $ printf "sleep time: %.2g\n" sleepTime
  when (sleepTime > 0) $
    CC.threadDelay (round (1e6 * sleepTime))


-- evaluate
forceRange :: XY (Double, Double) -> XY (Double, Double)
forceRange (XY (minX, maxX) (minY, maxY)) =
  minX `seq` maxX `seq` minY `seq` maxY `seq`
  (XY (minX, maxX) (minY, maxY))

largestRange :: XY (Double, Double) -> (Double, Double) -> XY (Double, Double)
largestRange (XY (minX, maxX) (minY, maxY)) (x, y) =
  forceRange $ XY (min minX x, max maxX x) (min minY y, max maxY y)

-- same behavior as 'Control.Concurrent.modifyMVar' with a different interface
modifyMVar' :: forall a . MVar a -> (a -> a) -> IO a
modifyMVar' mvar f = CC.modifyMVar mvar g
  where
    g :: a -> IO (a, a)
    g x = return (y, y)
      where
        y = f x
