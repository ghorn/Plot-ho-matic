{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.Plotter
       ( Plotter(..)
       , ChannelStuff(..)
       , runPlotter
       , execPlotter
       , tell
       ) where

import qualified GHC.Stats

import Control.Applicative
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Control.Concurrent as CC
import Data.Monoid
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import Text.Printf ( printf )
import System.Glib.Signals ( on )
import Prelude

-- | add channels to this, then run it with 'runPlotter'
newtype Plotter a = Plotter { unPlotter :: IO (a, [ChannelStuff]) } deriving Functor

instance Applicative Plotter where
  pure x = Plotter $ pure (x, [])
  f <*> v = Plotter $ liftA2 k (unPlotter f) (unPlotter v)
    where k ~(a, w) ~(b, w') = (a b, w `mappend` w')

instance Monad Plotter where
    return a = Plotter $ return (a, [])
    m >>= k  = Plotter $ do
        ~(a, w)  <- unPlotter m
        ~(b, w') <- unPlotter (k a)
        return (b, w `mappend` w')
    fail msg = Plotter $ fail msg

instance MonadIO Plotter where
  liftIO m = Plotter $ do
    a <- m
    return (a, mempty)

tell :: ChannelStuff -> Plotter ()
tell w = Plotter (return ((), [w]))

execPlotter :: Plotter a -> IO [ChannelStuff]
execPlotter m = do
    ~(_, w) <- unPlotter m
    return w

data ChannelStuff =
  ChannelStuff
  { csKillThreads :: IO ()
  , csMkChanEntry :: CC.MVar [Gtk.Window] -> IO Gtk.VBox
  }

-- | fire up the the GUI
runPlotter :: Plotter () -> IO ()
runPlotter plotterMonad = do
  statsEnabled <- GHC.Stats.getGCStatsEnabled

  void Gtk.initGUI
  void $ Gtk.timeoutAddFull (CC.yield >> return True) Gtk.priorityDefault 50

  -- start the main window
  win <- Gtk.windowNew
  void $ Gtk.set win
    [ Gtk.containerBorderWidth := 8
    , Gtk.windowTitle := "Plot-ho-matic"
    ]

  statsLabel <- Gtk.labelNew (Nothing :: Maybe String)
  let statsWorker = do
        CC.threadDelay 500000
        msg <- if statsEnabled
               then do
                 stats <- GHC.Stats.getGCStats
                 return $ printf "The current memory usage is %.2f MB"
                   ((realToFrac (GHC.Stats.currentBytesUsed stats) :: Double) /(1024*1024))
               else return "(enable GHC statistics with +RTS -T)"
        Gtk.postGUISync $ Gtk.labelSetText statsLabel ("Welcome to Plot-ho-matic!\n" ++ msg)
        statsWorker

  statsThread <- CC.forkIO statsWorker
  -- on close, kill all the windows and threads
  graphWindowsToBeKilled <- CC.newMVar []

  channels <- execPlotter plotterMonad
  let windows = map csMkChanEntry channels

  chanWidgets <- mapM (\x -> x graphWindowsToBeKilled) windows

  let killEverything :: IO ()
      killEverything = do
        CC.killThread statsThread
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ csKillThreads channels
        Gtk.mainQuit
  void $ on win Gtk.deleteEvent $ liftIO (killEverything >> return False)

  --------------- main widget -----------------
  -- button to clear history
  buttonDoNothing <- Gtk.buttonNewWithLabel "this button does absolutely nothing"
  void $ on buttonDoNothing Gtk.buttonActivated $
    putStrLn "seriously, it does nothing"

  -- box to hold list of channels
  channelBox <- Gtk.vBoxNew False 4
  Gtk.set channelBox $
    concatMap (\x -> [ Gtk.containerChild := x
                     , Gtk.boxChildPacking x := Gtk.PackNatural
                     ]) chanWidgets

  -- scroll to hold channel box
  scroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.scrolledWindowAddWithViewport scroll channelBox
  Gtk.set scroll [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
                 , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
                 ]

  -- vbox to hold everything
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox $
    [ Gtk.containerChild := statsLabel
    , Gtk.boxChildPacking statsLabel := Gtk.PackNatural
    , Gtk.containerChild := buttonDoNothing
    , Gtk.boxChildPacking buttonDoNothing := Gtk.PackNatural
    , Gtk.containerChild := scroll
    ]

  void $ Gtk.widgetSetSizeRequest vbox 20 200

  -- add widget to window and show
  void $ Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI
