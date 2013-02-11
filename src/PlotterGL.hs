{-# OPTIONS_GHC -Wall #-}

module PlotterGL where

import qualified Control.Concurrent as C
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.ModelView as New
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.Rendering.OpenGL as GL

import System.Glib.Signals (on)
import Data.List ( isPrefixOf )
import Data.Char ( toLower )

 
import Quotes ( PContainer(..), VarInfo(..) )
import DrawLine ( displayGraph )

data VarInfo' = VarInfo' { viName :: String
                         , viPc :: PContainer
                         , viMarked :: Bool
                         }

runPlotter :: [VarInfo] -> [C.ThreadId] -> IO ()
runPlotter infos backgroundThreads = do
  _ <- G.initGUI

  win <- G.windowNew
  graphWindows <- C.newMVar []
  let myQuit = do
        mapM_ C.killThread backgroundThreads
        gws <- C.readMVar graphWindows
        mapM_ Gtk.widgetDestroy gws
        G.mainQuit
  _ <- G.onDestroy win myQuit

  -- create a new tree model
  model <- G.listStoreNew $ map (\(VarInfo st pc) -> VarInfo' st pc False) infos
  view <- New.treeViewNewWithModel model

  New.treeViewSetHeadersVisible view True

  -- add three columns
  col1 <- New.treeViewColumnNew
  col2 <- New.treeViewColumnNew
  col3 <- New.treeViewColumnNew

  New.treeViewColumnSetTitle col1 "name"
  New.treeViewColumnSetTitle col2 "Int column"
  New.treeViewColumnSetTitle col3 "show?"

  renderer1 <- New.cellRendererTextNew
  renderer2 <- New.cellRendererTextNew
  renderer3 <- New.cellRendererToggleNew

  New.cellLayoutPackStart col1 renderer1 True
  New.cellLayoutPackStart col2 renderer2 True
  New.cellLayoutPackStart col3 renderer3 True

  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := viName row ]
  New.cellLayoutSetAttributes col2 renderer2 model $ \_ -> [ New.cellText := show "waaa" ]
  New.cellLayoutSetAttributes col3 renderer3 model $ \row -> [ New.cellToggleActive := viMarked row ]

  _ <- New.treeViewAppendColumn view col1
  _ <- New.treeViewAppendColumn view col2
  _ <- New.treeViewAppendColumn view col3

  -- update the model when the toggle buttons are activated
  _ <- on renderer3 G.cellToggled $ \pathStr -> do
    let (i:_) = G.stringToTreePath pathStr
    val <- G.listStoreGetValue model i
    G.listStoreSetValue model i val { viMarked = not (viMarked val) }


  -- enable interactive search
  New.treeViewSetEnableSearch view True
  New.treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    (i:_) <- G.treeModelGetPath model iter
    row <- G.listStoreGetValue model i
    return (map toLower str `isPrefixOf` map toLower (viName row))

  G.containerAdd win view
  G.widgetShowAll win

  glWin <- setupGlstuff infos
  C.modifyMVar_ graphWindows (return . (glWin:))
  
  G.mainGUI


setupGlstuff :: [VarInfo] -> IO Gtk.Window
setupGlstuff infos = do
--  _ <- Gtk.initGUI
 
  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  _ <- GtkGL.initGL
 
  -- We need a OpenGL frame buffer configuration to be able to create other
  -- OpenGL objects.
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]
 
  -- Create an OpenGL drawing area widget
  glCanvas <- GtkGL.glDrawingAreaNew glconfig
 
  _ <- Gtk.widgetSetSizeRequest glCanvas 250 250
 
  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  _ <- Gtk.onRealize glCanvas $ GtkGL.withGLDrawingArea glCanvas $ \_ -> do
    clearColor $= (Color4 0.0 0.0 0.0 0.0)
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers
 
  -- Set the repaint handler
  _ <- Gtk.onExpose glCanvas $ \_ -> do
    GtkGL.withGLDrawingArea glCanvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      displayGraph infos
      GtkGL.glDrawableSwapBuffers glwindow
    return True
 
  -- Setup the animation
  _ <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw glCanvas
      return True)
    Gtk.priorityDefaultIdle animationWaitTime
 
  --------------------------------
  -- Setup the rest of the GUI:
  --
  window <- Gtk.windowNew
  _ <- Gtk.onDestroy window (putStrLn "gl window destroyed")
  _ <- Gtk.set window [ Gtk.containerBorderWidth := 8
                      , Gtk.windowTitle := "A graph, yo"
                      ]
 
  vbox <- Gtk.vBoxNew False 4
  _ <- Gtk.set window [ Gtk.containerChild := vbox ]
 
  label <- Gtk.labelNew (Just "Gtk2Hs using OpenGL via HOpenGL!")
  button <- Gtk.buttonNewWithLabel "Close"
  _ <- Gtk.onClicked button (Gtk.widgetDestroy window)
  Gtk.set vbox [ Gtk.containerChild := glCanvas,
                 Gtk.containerChild := label,
                 Gtk.containerChild := button ]
 
  Gtk.widgetShowAll window
  return window

 
animationWaitTime :: Int
animationWaitTime = 3
