{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}

module PlotHo
       ( Plotter
       , runPlotter
       , XAxisType(..)
       , addHistoryChannel
       , addChannel
         -- * re-exported for convenience
       , Lookup
       ) where

import qualified GHC.Stats

import Control.Applicative ( Applicative(..), liftA2 )
import Control.Lens ( (^.) )
import Data.Monoid ( mappend, mempty )
import Control.Monad ( when )
import qualified Control.Concurrent as CC
import qualified Data.Foldable as F
import qualified Data.IORef as IORef
import Data.Time ( NominalDiffTime, getCurrentTime, diffUTCTime )
import Data.Tree ( Tree )
import qualified Data.Tree as Tree
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import Text.Printf ( printf )
import Text.Read ( readMaybe )
import System.Glib.Signals ( on )
--import System.IO ( withFile, IOMode ( WriteMode ) )
--import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence as S

import Accessors

import PlotHo.PlotTypes ( Channel(..) )
import PlotHo.GraphWidget ( newGraph )

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
  }

-- | Simplified time-series channel which passes a "send message" function to a worker and forks it using 'forkIO'.
-- The plotter will plot a time series of messages sent by the worker.
-- The worker should pass True to reset the message history, so sending True the first message and False subsequent messages is a good starting place.
-- You will have to recompile the plotter if the types change.
-- If you don't want to do this, use the more generic "addChannel" interface
-- and use a type like a Tree to represent your data.
addHistoryChannel ::
  Lookup a
  => String -- ^ channel name
  -> XAxisType -- ^ what to use for the X axis
  -> ((a -> Bool -> IO ()) -> IO ()) -- ^ worker which is passed a "new message" function, this will be forked with 'forkIO'
  -> Plotter ()
addHistoryChannel name xaxisType action = do
  (chan, newMessage) <- liftIO $ newHistoryChannel name xaxisType
  workerTid <- liftIO $ CC.forkIO (action newMessage)
  tell ChannelStuff { csKillThreads = CC.killThread workerTid
                    , csMkChanEntry = newChannelWidget chan
                    }

-- | This is the general interface to plot whatever you want.
-- Use this when you want to give the whole time series in one go, rather than one at a time
-- such as with 'addHistoryChannel'.
-- Using types or data, you must encode the signal tree with the message so that
-- the plotter can build you the nice message toggle tree.
addChannel ::
  String -- ^ channel name
  -> (a -> a -> Bool) -- ^ Is the signal tree the same? This is used for instance if signals have changed and the plotter needs to rebuild the signal tree. This lets you keep the plotter running and change other programs which send messages to the plotter.
  -> (a -> [Tree (String, String, Maybe (a -> [[(Double, Double)]]))]) -- ^ how to build the signal tree
  -> ((a -> IO ()) -> IO ()) -- ^ worker which is passed a "new message" function, this will be forked with 'forkIO'
  -> Plotter ()
addChannel name sameSignalTree toSignalTree action = do
  (chan, newMessage) <- liftIO $ newChannel name sameSignalTree toSignalTree
  workerTid <- liftIO $ CC.forkIO (action newMessage)
  tell ChannelStuff { csKillThreads = CC.killThread workerTid
                    , csMkChanEntry = newChannelWidget chan
                    }


newChannel ::
  forall a
  . String
  -> (a -> a -> Bool)
  -> (a -> [Tree (String, String, Maybe (a -> [[(Double, Double)]]))])
  -> IO (Channel a, a -> IO ())
newChannel name sameSignalTree toSignalTree = do
  msgStore <- Gtk.listStoreNew []
  maxHist <- IORef.newIORef 0

  let newMessage :: a -> IO ()
      newMessage next = do
        -- grab the time and counter
        Gtk.postGUIAsync $ do
          size <- Gtk.listStoreGetSize msgStore
          if size == 0
            then Gtk.listStorePrepend msgStore next
            else Gtk.listStoreSetValue msgStore 0 next

  let retChan = Channel { chanName = name
                        , chanMsgStore = msgStore
                        , chanSameSignalTree = sameSignalTree
                        , chanToSignalTree = toSignalTree
                        , chanMaxHistory = maxHist
                        }

  return (retChan, newMessage)


data History a = History (S.Seq (a, Int, NominalDiffTime))

type SignalTree a = Tree.Forest (String, String, Maybe (History a -> [[(Double, Double)]]))

data XAxisType =
  XAxisTime -- ^ time since the first message
  | XAxisTime0 -- ^ time since the first message, normalized to 0 (to reduce plot jitter)
  | XAxisCount -- ^ message index
  | XAxisCount0 -- ^ message index, normalized to 0 (to reduce plot jitter)

historySignalTree :: forall a . Lookup a => XAxisType -> SignalTree a
historySignalTree axisType = case accessors of
  (Field _) -> error "historySignalTree: got a Field right away"
  d -> Tree.subForest $ head $ makeSignalTree' "" "" d
  where
    makeSignalTree' :: String -> String -> AccessorTree a -> SignalTree a
    makeSignalTree' myName parentName (Data (pn,_) children) =
      [Tree.Node
       (myName, parentName, Nothing)
       (concatMap (\(getterName,child) -> makeSignalTree' getterName pn child) children)
      ]
    makeSignalTree' myName parentName (Field field) =
      [Tree.Node (myName, parentName, Just (toHistoryGetter (toDoubleGetter field))) []]

    toDoubleGetter :: Field a -> (a -> Double)
    toDoubleGetter (FieldDouble f) = (^. f)
    toDoubleGetter (FieldFloat f) = realToFrac . (^. f)
    toDoubleGetter (FieldBool f) = fromIntegral . fromEnum . (^. f)
    toDoubleGetter (FieldInt f) = fromIntegral . (^. f)
    toDoubleGetter (FieldString _) = const 0
    toDoubleGetter FieldSorry = const 0

    toHistoryGetter :: (a -> Double) -> History a -> [[(Double, Double)]]
    toHistoryGetter = case axisType of
      XAxisTime   -> timeGetter
      XAxisTime0  -> timeGetter0
      XAxisCount  -> countGetter
      XAxisCount0 -> countGetter0

    timeGetter  get (History s) = [map (\(val, _, time) -> (realToFrac time, get val)) (F.toList s)]
    timeGetter0 get (History s) = [map (\(val, _, time) -> (realToFrac time - time0, get val)) (F.toList s)]
      where
        time0 :: Double
        time0 = case S.viewl s of
          (_, _, time0') S.:< _ -> realToFrac time0'
          S.EmptyL -> 0
    countGetter  get (History s) = [map (\(val, k, _) -> (fromIntegral k, get val)) (F.toList s)]
    countGetter0 get (History s) = [map (\(val, k, _) -> (fromIntegral k - k0, get val)) (F.toList s)]
      where
        k0 :: Double
        k0 = case S.viewl s of
          (_, k0', _) S.:< _ -> realToFrac k0'
          S.EmptyL -> 0

-- History channel which automatically generates the signal tree for you
-- based on the Lookup instance. You have to recompile the plotter if
-- the types change.
-- This is the internal part which should be wrapped by addHistoryChannel.
newHistoryChannel ::
  forall a
  . Lookup a
  => String
  -> XAxisType
  -> IO (Channel (History a), a -> Bool -> IO ())
newHistoryChannel name xaxisType = do
  time0 <- getCurrentTime >>= IORef.newIORef
  counter <- IORef.newIORef 0
  maxHist <- IORef.newIORef 200

  msgStore <- Gtk.listStoreNew []

  let newMessage :: a -> Bool -> IO ()
      newMessage next reset = do
        -- grab the time and counter
        time <- getCurrentTime
        when reset $ do
          IORef.writeIORef time0 time
          IORef.writeIORef counter 0

        k <- IORef.readIORef counter
        time0' <- IORef.readIORef time0

        IORef.writeIORef counter (k+1)
        Gtk.postGUIAsync $ do
          let val = (next, k, diffUTCTime time time0')
          size <- Gtk.listStoreGetSize msgStore
          if size == 0
            then Gtk.listStorePrepend msgStore (History (S.singleton val))
            else do History vals0 <- Gtk.listStoreGetValue msgStore 0
                    maxHistory <- IORef.readIORef maxHist
                    let undropped = vals0 S.|> val
                        dropped = S.drop (S.length undropped - maxHistory) undropped
                    Gtk.listStoreSetValue msgStore 0 (History dropped)

          when reset $ Gtk.listStoreSetValue msgStore 0 (History (S.singleton val))

  let tst :: History a -> [Tree ( String
                                , String
                                , Maybe (History a -> [[(Double, Double)]])
                                )]
      tst = const (historySignalTree xaxisType)

  let retChan = Channel { chanName = name
                        , chanMsgStore = msgStore
                        , chanSameSignalTree = \_ _ -> True
                        , chanToSignalTree = tst
                        , chanMaxHistory = maxHist
                        }

  return (retChan, newMessage)

-- | fire up the the GUI
runPlotter :: Plotter () -> IO ()
runPlotter plotterMonad = do
  statsEnabled <- GHC.Stats.getGCStatsEnabled

  _ <- Gtk.initGUI
  _ <- Gtk.timeoutAddFull (CC.yield >> return True) Gtk.priorityDefault 50

  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
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



  let killEverything = do
        CC.killThread statsThread
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ csKillThreads channels
        Gtk.mainQuit
  _ <- Gtk.onDestroy win killEverything

  --------------- main widget -----------------
  -- button to clear history
  buttonDoNothing <- Gtk.buttonNewWithLabel "this button does absolutely nothing"
  _ <- Gtk.onClicked buttonDoNothing $
       putStrLn "seriously, it does nothing"

  -- vbox to hold buttons and list of channel
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox $
    [ Gtk.containerChild := statsLabel
    , Gtk.boxChildPacking statsLabel := Gtk.PackNatural
    , Gtk.containerChild := buttonDoNothing
    ] ++ concatMap (\x -> [ Gtk.containerChild := x
                          , Gtk.boxChildPacking x := Gtk.PackNatural
                          ]) chanWidgets

  -- add widget to window and show
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI


-- the list of channels
newChannelWidget :: Channel a
                    -> CC.MVar [Gtk.Window] -> IO Gtk.VBox
newChannelWidget channel graphWindowsToBeKilled = do
  vbox <- Gtk.vBoxNew False 4

  nameBox' <- Gtk.hBoxNew False 4
  nameBox <- labeledWidget (chanName channel) nameBox'

  buttonsBox <- Gtk.hBoxNew False 4

  -- button to clear history
  buttonAlsoDoNothing <- Gtk.buttonNewWithLabel "also do nothing"
--  _ <- Gtk.onClicked buttonAlsoDoNothing $ do
--    putStrLn "i promise, nothing happens"
--    -- CC.modifyMVar_ logData (const (return S.empty))
--    return ()
  let triggerYo action = Gtk.onClicked buttonAlsoDoNothing action >> return ()

  -- button to make a new graph
  buttonNew <- Gtk.buttonNewWithLabel "new graph"
  _ <- Gtk.onClicked buttonNew $ do
    graphWin <- newGraph
                triggerYo
                (chanName channel)
                (chanSameSignalTree channel)
                (chanToSignalTree channel)
                (chanMsgStore channel)

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
        let reset = Gtk.entrySetText entryEntry "(max)"
        case readMaybe txt :: Maybe Int of
          Nothing ->
            putStrLn ("max history: couldn't make an Int out of \"" ++ show txt ++ "\"") >> reset
          Just 0  -> putStrLn ("max history: must be greater than 0") >> reset
          Just k  -> IORef.writeIORef (chanMaxHistory channel) k

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
                     , Gtk.containerChild := buttonAlsoDoNothing
                     , Gtk.boxChildPacking buttonAlsoDoNothing := Gtk.PackNatural
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
