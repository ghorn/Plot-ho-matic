{-# OPTIONS_GHC -Wall #-}

module PlotterGL where

import qualified Control.Concurrent as C
import Control.Monad ( zipWithM_ )
--import qualified Data.Foldable as F
import Data.Sequence ( Seq )
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.Rendering.OpenGL ( ($=) )
import qualified Graphics.Rendering.OpenGL as GL
import System.Glib.Signals (on)

 
import Colors ( niceColors )
import DrawLine ( drawLine )
import PlotTypes ( PContainer(..), VarInfo(..), clearVarInfo, toFrac )

data ListViewInfo = ListViewInfo String (C.MVar PContainer) Bool

-- what the graph should draw
data GraphInfo = GraphInfo [(String, C.MVar PContainer)]


runPlotter :: [VarInfo] -> [C.ThreadId] -> IO ()
runPlotter infos backgroundThreadsToKill = do
  _ <- Gtk.initGUI
 
  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  _ <- GtkGL.initGL
 
  -- We need a OpenGL frame buffer configuration to be able to create other
  -- OpenGL objects.
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]

  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "Plot-ho-matic"
                   ]
  
  graphWindowsToBeKilled <- C.newMVar []
  let myQuit = do
        gws <- C.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ C.killThread backgroundThreadsToKill
        Gtk.mainQuit
  _ <- Gtk.onDestroy win myQuit

  buttonQuit <- Gtk.buttonNewWithLabel "QUIT"
  _ <- Gtk.onClicked buttonQuit (Gtk.widgetDestroy win)

  buttonClear <- Gtk.buttonNewWithLabel "Clear All Values"
  _ <- Gtk.onClicked buttonClear $ mapM_ clearVarInfo infos

  -- button to create a new graph
  buttonNewGraph <- Gtk.buttonNewWithLabel "New Graph"
  _ <- Gtk.onClicked buttonNewGraph (newGraph graphWindowsToBeKilled infos glconfig)

  -- vbox to hold buttons
  vbox <- Gtk.vBoxNew False 4
--  Gtk.containerAdd win button
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.set vbox [ Gtk.containerChild := buttonQuit
               , Gtk.containerChild := buttonNewGraph
               , Gtk.containerChild := buttonClear
               ]

  Gtk.widgetShowAll win
  
  Gtk.mainGUI


-- make a new graph window
newGraph :: C.MVar [Gtk.Window] -> [VarInfo] -> GtkGL.GLConfig -> IO ()
newGraph graphWindowsToBeKilled infos glconfig = do
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "I am a graph"
                   ]

  -- create a new tree model
  model <- Gtk.listStoreNew $ map (\(VarInfo st pc) -> ListViewInfo st pc False) infos
  view <- Gtk.treeViewNewWithModel model

  Gtk.treeViewSetHeadersVisible view True

  -- add three columns
  col1 <- Gtk.treeViewColumnNew
  col2 <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle col1 "name"
  Gtk.treeViewColumnSetTitle col2 "visible?"

  renderer1 <- Gtk.cellRendererTextNew
  renderer2 <- Gtk.cellRendererToggleNew

  Gtk.cellLayoutPackStart col1 renderer1 True
  Gtk.cellLayoutPackStart col2 renderer2 True

  Gtk.cellLayoutSetAttributes col1 renderer1 model $ \(ListViewInfo name _ _) -> [ Gtk.cellText := name]
  Gtk.cellLayoutSetAttributes col2 renderer2 model $ \(ListViewInfo _ _ marked) -> [ Gtk.cellToggleActive := marked ]

  _ <- Gtk.treeViewAppendColumn view col1
  _ <- Gtk.treeViewAppendColumn view col2

  -- update the model when the toggle buttons are activated
  graphInfoMVar <- C.newMVar (GraphInfo [])
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    -- toggle the check mark
    let (i:_) = Gtk.stringToTreePath pathStr
    (ListViewInfo n p val) <- Gtk.listStoreGetValue model i
    Gtk.listStoreSetValue model i (ListViewInfo n p (not val))

    -- update the graph information
    varinfos' <- Gtk.listStoreToList model
    let newGraphInfo = GraphInfo [(str, pc) | (ListViewInfo str pc marked) <- varinfos', marked]
    _ <- C.swapMVar graphInfoMVar newGraphInfo
    return ()

  let displayFun = do
        gi <- C.readMVar graphInfoMVar
        displayGraph gi
  canvas <- makeGraphCanvas glconfig displayFun

  -- vbox to hold treeview and gl drawing
  hbox <- Gtk.hBoxNew False 4
  _ <- Gtk.set win [ Gtk.containerChild := hbox ]
  Gtk.set hbox [ Gtk.containerChild := view
               , Gtk.containerChild := canvas
               ]

  C.modifyMVar_ graphWindowsToBeKilled (return . (win:))

  Gtk.widgetShowAll win
  
  return ()



makeGraphCanvas :: GtkGL.GLConfig -> IO () -> IO GtkGL.GLDrawingArea
makeGraphCanvas glconfig displayFun = do
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
      _ <- displayFun
      GtkGL.glDrawableSwapBuffers glwindow
    return True
 
  -- Setup the animation
  _ <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle animationWaitTime

  return canvas

 
animationWaitTime :: Int
animationWaitTime = 3


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
