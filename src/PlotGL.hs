{-# OPTIONS_GHC -Wall #-}

module PlotGL ( makeGraphCanvas, displayGraph ) where

import qualified Control.Concurrent as C
import Control.Monad ( zipWithM_ )
import qualified Data.Foldable as F
import           Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import           Graphics.Rendering.OpenGL ( ($=) )
import qualified Graphics.Rendering.OpenGL as GL

import Colors ( niceColors )
import PlotTypes ( GraphInfo(..), toFrac )

linspace :: Fractional a => a -> a -> Int -> [a]
linspace x0 x1 n =
  map ((+ x0) . (((x1-x0)/((realToFrac n)-1)) *) . realToFrac) [0..(n-1)]

drawLine :: (Fractional a, Real a) => Seq a -> IO ()
drawLine seq' = do
  let maxval = F.foldl' (\acc x -> max acc (abs x)) (1e-12) seq'
      num = Seq.length seq'
      xs = linspace (-1) 1 num
      ys = map ((/ (realToFrac maxval)) . realToFrac) $ F.toList seq'
--      ys = map realToFrac $ F.toList seq'

  GL.renderPrimitive GL.LineStrip $ do
    zipWithM_ (\x y -> GL.vertex (GL.Vertex3 x y 0.0 :: GL.Vertex3 GL.GLfloat)) xs ys


makeGraphCanvas :: Int -> GtkGL.GLConfig -> IO () -> IO GtkGL.GLDrawingArea
makeGraphCanvas animationWaitTime glconfig displayFun = do
  -- Create an OpenGL drawing area widget
  canvas <- GtkGL.glDrawingAreaNew glconfig
 
  _ <- Gtk.widgetSetSizeRequest canvas 250 250
 
  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  _ <- Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    GL.clearColor $= (GL.Color4 0.0 0.0 0.0 0.0)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    GL.depthFunc $= Just GL.Less
    GL.drawBuffer $= GL.BackBuffers
 
  -- Set the repaint handler
  _ <- Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      displayFun
      GtkGL.glDrawableSwapBuffers glwindow
    return True
 
  -- Setup the animation
  _ <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle animationWaitTime

  return canvas


-- Draw the OpenGL polygon.
displayGraph :: GraphInfo -> IO ()
displayGraph (GraphInfo gis) = do
--  let printLog = mapM_ printVarInfo infos
--  printLog
  GL.loadIdentity
  GL.color (GL.Color3 1 1 1 :: GL.Color3 GL.GLfloat)
  let f (name,mv) = do
        pc <- C.readMVar mv
        return (name,toFrac pc :: Seq GL.GLfloat)
  namePc <- mapM f gis
  
--  let getMax seq' = F.foldl' max (-1e100) seq'
--      getMin seq' = F.foldl' min (1e100) seq'
--
--      max' = maximum $ 1e100 : map (getMax . snd) namePc
--      min' = minimum $ -1e100 : map (getMin . snd) namePc
--  
--  GL.matrixMode $= GL.Projection
--  GL.loadIdentity
--  GL.ortho 0.0 1.0 min' max' (-1.0) 1.0
    
  let drawOne (_,pc) (cr,cg,cb) = do
        GL.color (GL.Color3 cr cg cb)
        drawLine pc
  zipWithM_ drawOne namePc niceColors
