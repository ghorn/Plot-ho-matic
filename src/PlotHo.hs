{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language PackageImports #-}

module PlotHo
       ( Lookup(..)
       , SignalTree
       , AccessorTree(..)
       , addChannel
       , makeSignalTree
       , runPlotter
       ) where

import Data.Monoid
import Data.Time ( NominalDiffTime )
import qualified Data.Sequence as S
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import Data.Time ( getCurrentTime, diffUTCTime )
import "gtk" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk" Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
--import System.IO ( withFile, IOMode ( WriteMode ) )
--import qualified Data.ByteString.Lazy as BSL
import qualified Data.Tree as Tree
import Text.Printf ( printf )

import Control.Applicative ( Applicative(..), liftA2 )

import qualified GHC.Stats

import PlotTypes --( Channel(..), SignalTree )
import Accessors
import GraphWidget ( newGraph )
import ReadMaybe ( readMaybe )

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

liftIO :: IO a -> Plotter a
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
  , csClearChan :: IO ()
  }


makeSignalTree :: Lookup a => a -> SignalTree a
makeSignalTree x = case accessors x of
  (ATGetter _) -> error "makeSignalTree: got an accessor right away"
  d -> Tree.subForest $ head $ makeSignalTree' "" "" d
  where
    makeSignalTree' :: String -> String -> AccessorTree a -> SignalTree a
    makeSignalTree' myName parentName (Data (pn,_) children) =
      [Tree.Node
       (myName, parentName, Nothing)
       (concatMap (\(getterName,child) -> makeSignalTree' getterName pn child) children)
      ]
    makeSignalTree' myName parentName (ATGetter getter) =
      [Tree.Node (myName, parentName, Just (Left getter)) []]


addChannel :: String -> SignalTree a
              -> ((a -> IO ()) -> (SignalTree a -> IO ()) -> IO ())
              -> Plotter ()
addChannel name signalTree0 action = do
  chanStuff <- liftIO $ newChannel name signalTree0 action
  tell chanStuff


newChannel :: forall a .
              String -> SignalTree a
              -> ((a -> IO ()) -> (SignalTree a -> IO ()) -> IO ())
              -> IO ChannelStuff
newChannel name signalTree0 action = do
  time0 <- getCurrentTime

  trajChan <- STM.atomically STM.newTQueue
  trajMv <- CC.newMVar S.empty
  maxHistMv <- CC.newMVar 200

  signalTreeStore <- Gtk.listStoreNew []

  let getLastValue :: IO a
      getLastValue = do
        val <- STM.atomically (STM.readTQueue trajChan)
        empty <- STM.atomically (STM.isEmptyTQueue trajChan)
        if empty then return val else getLastValue


  let rebuildSignalTree newSignalTree = do
        --putStrLn $ "rebuilding signal tree"
        size <- Gtk.listStoreGetSize signalTreeStore
        if size == 0
          then Gtk.listStorePrepend signalTreeStore newSignalTree
          else Gtk.listStoreSetValue signalTreeStore 0 newSignalTree

  -- this is the loop that reads new messages and stores them
  let serverLoop :: Int -> IO ()
      serverLoop k = do
        -- wait until a new message is written to the Chan
        newPoint0 <- getLastValue

        -- grab the timestamp
        time <- getCurrentTime

        -- write to the mvar
        maxHist <- CC.readMVar maxHistMv
        let newPoint = (newPoint0, k, diffUTCTime time time0)
            addPoint lst0 = S.drop (S.length lst0 - maxHist + 1) (lst0 S.|> newPoint)
        CC.modifyMVar_ trajMv (return . addPoint)
        return ()

        -- loop forever
        serverLoop (k+1)

  let updateMaxHist k = CC.modifyMVar_ maxHistMv (const (return k))

  rebuildSignalTree signalTree0
  serverTid <- CC.forkIO (serverLoop 0)
  let writeToThread = STM.atomically . STM.writeTQueue trajChan


  p <- CC.forkIO (action writeToThread (Gtk.postGUISync . rebuildSignalTree))

  return $
    ChannelStuff
    { csKillThreads = mapM_ CC.killThread [serverTid,p]
    , csMkChanEntry = newChannelWidget trajMv signalTreeStore updateMaxHist name
    , csClearChan = CC.modifyMVar_ trajMv (const (return  S.empty))
    }


runPlotter :: Plotter () -> IO ()
runPlotter plotterMonad = do
  statsEnabled <- GHC.Stats.getGCStatsEnabled

  _ <- Gtk.initGUI

  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "Plot-ho-matic"
                   ]

  -- on close, kill all the windows and threads
  graphWindowsToBeKilled <- CC.newMVar []

  -- run the plotter monad
  channels <- execPlotter plotterMonad
  let windows = map csMkChanEntry channels

  chanWidgets <- mapM (\x -> x graphWindowsToBeKilled) windows

  -- ghc stats
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

  let killEverything = do
        CC.killThread statsThread
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ csKillThreads channels
        Gtk.mainQuit
  _ <- Gtk.onDestroy win killEverything

  --------------- main widget -----------------
  -- button to clear history
  buttonClear <- Gtk.buttonNewWithLabel "clear history"
  _ <- Gtk.onClicked buttonClear $ do
    mapM_ csClearChan channels



  -- vbox to hold buttons
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox $
    [ Gtk.containerChild := statsLabel
    , Gtk.boxChildPacking statsLabel := Gtk.PackNatural
    , Gtk.containerChild := buttonClear
    , Gtk.boxChildPacking buttonClear := Gtk.PackNatural
    ] ++ concatMap (\x -> [Gtk.containerChild := x
                          , Gtk.boxChildPacking x := Gtk.PackNatural
                          ]) chanWidgets

  -- add widget to window and show
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI

-- helper to make an hbox with a label
labeledWidget :: Gtk.WidgetClass a => String -> a -> IO Gtk.HBox
labeledWidget name widget = do
  label <- Gtk.labelNew (Just name)
  hbox <- Gtk.hBoxNew False 4
  Gtk.set hbox [ Gtk.containerChild := label
               , Gtk.containerChild := widget
               , Gtk.boxChildPacking label := Gtk.PackNatural
--               , Gtk.boxChildPacking widget := Gtk.PackNatural
               ]
  return hbox

-- the list of channels
newChannelWidget ::
  CC.MVar (S.Seq (a, Int, NominalDiffTime))
  -> Gtk.ListStore (SignalTree a)
  -> (Int -> IO ())
  -> String
  -> CC.MVar [Gtk.Window]
  -> IO Gtk.VBox
newChannelWidget logData signalTreeStore updateMaxHistMVar name graphWindowsToBeKilled = do
  vbox <- Gtk.vBoxNew False 4

  nameBox' <- Gtk.hBoxNew False 4
  nameBox <- labeledWidget name nameBox'

  buttonsBox <- Gtk.hBoxNew False 4

  -- button to clear history
  buttonClear <- Gtk.buttonNewWithLabel "clear history"
  _ <- Gtk.onClicked buttonClear $ do
    CC.modifyMVar_ logData (const (return S.empty))
    return ()

  -- button to make a new graph
  buttonNew <- Gtk.buttonNewWithLabel "new graph"
  _ <- Gtk.onClicked buttonNew $ do
    graphWin <- newGraph name signalTreeStore logData

    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))

  -- entry to set history length
  entryAndLabel <- Gtk.hBoxNew False 4
  entryLabel <- Gtk.vBoxNew False 4 >>= labeledWidget "max history:"
  entryEntry <- Gtk.entryNew
  Gtk.set entryEntry [ Gtk.entryEditable := True
                     , Gtk.widgetSensitive := True
                     ]
  Gtk.entrySetText entryEntry "200"
  let updateMaxHistory = do
        txt <- Gtk.get entryEntry Gtk.entryText
        case readMaybe txt of
          Just k -> updateMaxHistMVar k
          Nothing -> putStrLn $ "max history: couldn't make an Int out of \"" ++ show txt ++ "\""
  _ <- on entryEntry Gtk.entryActivate updateMaxHistory
  updateMaxHistory


  Gtk.set entryAndLabel [ Gtk.containerChild := entryLabel
                        , Gtk.boxChildPacking entryLabel := Gtk.PackNatural
                        , Gtk.containerChild := entryEntry
                        , Gtk.boxChildPacking entryEntry := Gtk.PackNatural
                        ]


  -- put all the buttons/entries together
  Gtk.set buttonsBox [ Gtk.containerChild := buttonNew
                     , Gtk.boxChildPacking buttonNew := Gtk.PackNatural
                     , Gtk.containerChild := buttonClear
                     , Gtk.boxChildPacking buttonClear := Gtk.PackNatural
                     , Gtk.containerChild := entryAndLabel
                     , Gtk.boxChildPacking entryAndLabel := Gtk.PackNatural
                     ]

  Gtk.set vbox [ Gtk.containerChild := nameBox
               , Gtk.boxChildPacking   nameBox := Gtk.PackNatural
               , Gtk.containerChild := buttonsBox
               , Gtk.boxChildPacking   buttonsBox := Gtk.PackNatural
               ]

  return vbox


----  -- save all channel data when this button is pressed
----  _ <- on renderer3 Gtk.cellToggled $ \pathStr -> do
----    let (i:_) = Gtk.stringToTreePath pathStr
----    lv <- Gtk.listStoreGetValue model i
----    let writerThread = do
----          bct <- chanGetByteStrings (lvChan lv)
----          let filename = chanName (lvChan lv) ++ "_log.dat"
----              blah _      sizes [] = return (reverse sizes)
----              blah handle sizes ((x,_,_):xs) = do
----                BSL.hPut handle x
----                blah handle (BSL.length x : sizes) xs
----          putStrLn $ "trying to write file \"" ++ filename ++ "\"..."
----          sizes <- withFile filename WriteMode $ \handle -> blah handle [] bct
----          putStrLn $ "finished writing file, wrote " ++ show (length sizes) ++ " protos"
----
----          putStrLn "writing file with sizes..."
----          writeFile (filename ++ ".sizes") (unlines $ map show sizes)
----          putStrLn "done"
----    _ <- CC.forkIO writerThread
--    return ()
--
--  return treeview
