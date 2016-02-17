{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.Channels
       ( XAxisType(..)
       , addHistoryChannel
       , Meta
       , addHistoryChannel'
       , addChannel
       ) where

import Control.Lens ( (^.) )
import Control.Monad ( void, when )
import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Control.Concurrent as CC
import qualified Data.Foldable as F
import qualified Data.IORef as IORef
import Data.Time ( NominalDiffTime, getCurrentTime, diffUTCTime )
import Data.Tree ( Tree )
import qualified Data.Tree as Tree
import Data.Vector ( Vector )
import qualified Data.Vector as V
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import Text.Read ( readMaybe )
import System.Glib.Signals ( on )
--import System.IO ( withFile, IOMode ( WriteMode ) )
--import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence as S

import Accessors

import PlotHo.GraphWidget ( newGraph )
import PlotHo.Plotter ( Plotter, ChannelStuff(..), tell )
import PlotHo.PlotTypes ( Channel(..), PlotterOptions(..) )

-- | Simplified time-series channel which passes a "send message" function to a worker and forks it using 'forkIO'.
-- The plotter will plot a time series of messages sent by the worker.
-- The worker should pass True to reset the message history, so sending True the first message and False subsequent messages is a good starting place.
-- You will have to recompile the plotter if the types change.
-- If you don't want to do this, use the more generic "addChannel" interface
-- and use a type like a Tree to represent your data, or use the "addHistoryChannel'" function.
addHistoryChannel ::
  Lookup a
  => PlotterOptions -- ^ some options
  -> String -- ^ channel name
  -> XAxisType -- ^ what to use for the X axis
  -> ((a -> Bool -> IO ()) -> IO ()) -- ^ worker which is passed a "new message" function, this will be forked with 'forkIO'
  -> Plotter ()
addHistoryChannel plotterOptions name xaxisType action = do
  (chan, newMessage) <- liftIO $ newHistoryChannel name xaxisType
  workerTid <- liftIO $ CC.forkIO (action newMessage)
  tell ChannelStuff { csKillThreads = CC.killThread workerTid
                    , csMkChanEntry = newChannelWidget plotterOptions chan
                    }

-- | Dynamic time-series channel which can change its signal tree without recompiling the plotter.
addHistoryChannel' ::
  PlotterOptions -- ^ some options
  -> String -- ^ channel name
  -> ((Double -> Vector Double -> Maybe Meta -> IO ()) -> IO ()) -- ^ worker which is passed a "new message" function, this will be forked with 'forkIO'
  -> Plotter ()
addHistoryChannel' plotterOptions name action = do
  (chan, newMessage) <- liftIO $ newHistoryChannel' name
  workerTid <- liftIO $ CC.forkIO (action newMessage)
  tell ChannelStuff { csKillThreads = CC.killThread workerTid
                    , csMkChanEntry = newChannelWidget plotterOptions chan
                    }

-- | This is the general interface to plot whatever you want.
-- Use this when you want to give the whole time series in one go, rather than one at a time
-- such as with 'addHistoryChannel'.
-- Using types or data, you must encode the signal tree with the message so that
-- the plotter can build you the nice message toggle tree.
addChannel ::
  PlotterOptions -- ^ some options
  -> String -- ^ channel name
  -> (a -> a -> Bool) -- ^ Is the signal tree the same? This is used for instance if signals have changed and the plotter needs to rebuild the signal tree. This lets you keep the plotter running and change other programs which send messages to the plotter.
  -> (a -> [Tree ([String], Either String (a -> [[(Double, Double)]]))]) -- ^ how to build the signal tree
  -> ((a -> IO ()) -> IO ()) -- ^ worker which is passed a "new message" function, this will be forked with 'forkIO'
  -> Plotter ()
addChannel plotterOptions name sameSignalTree toSignalTree action = do
  (chan, newMessage) <- liftIO $ newChannel name sameSignalTree toSignalTree
  workerTid <- liftIO $ CC.forkIO (action newMessage)
  tell ChannelStuff { csKillThreads = CC.killThread workerTid
                    , csMkChanEntry = newChannelWidget plotterOptions chan
                    }


newChannel ::
  forall a
  . String
  -> (a -> a -> Bool)
  -> (a -> [Tree ([String], Either String (a -> [[(Double, Double)]]))])
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
type HistorySignalTree a = Tree.Forest ([String], Either String (History a -> [[(Double, Double)]]))

data XAxisType =
  XAxisTime -- ^ time since the first message
  | XAxisTime0 -- ^ time since the first message, normalized to 0 (to reduce plot jitter)
  | XAxisCount -- ^ message index
  | XAxisCount0 -- ^ message index, normalized to 0 (to reduce plot jitter)

historySignalTree :: forall a . Lookup a => XAxisType -> HistorySignalTree a
historySignalTree axisType = case accessors of
  Left _ -> error "historySignalTree: got a Field right away"
  acc -> Tree.subForest $ head $ makeSignalTree' [] acc
  where
    makeSignalTree' :: [String] -> AccessorTree a -> HistorySignalTree a
    makeSignalTree' myFieldName (Right (GAData _ (GAConstructor cname children))) =
      [Tree.Node
       (reverse myFieldName, Left cname)
       (concatMap (\(getterName, child) -> makeSignalTree' (fromMName getterName:myFieldName) child) children)
      ]
    makeSignalTree' myFieldName (Right (GAData _ (GASum enum))) =
      [Tree.Node (reverse myFieldName, Right (toHistoryGetter (fromIntegral . eToIndex enum))) []]
    makeSignalTree' myFieldName (Left field) =
      [Tree.Node (reverse myFieldName, Right (toHistoryGetter (toDoubleGetter field))) []]
    fromMName (Just x) = x
    fromMName Nothing = "()"

    toDoubleGetter :: GAField a -> (a -> Double)
    toDoubleGetter (FieldDouble f) = (^. f)
    toDoubleGetter (FieldFloat f) = realToFrac . (^. f)
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
  maxHist <- IORef.newIORef 500

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
                    let dropped = S.drop (1 + S.length vals0 - maxHistory) (vals0 S.|> val)
                    Gtk.listStoreSetValue msgStore 0 (History dropped)

          when reset $ Gtk.listStoreSetValue msgStore 0 (History (S.singleton val))

  let tst :: History a -> [Tree ( [String]
                                , Either String (History a -> [[(Double, Double)]])
                                )]
      tst = const (historySignalTree xaxisType)

  let retChan = Channel { chanName = name
                        , chanMsgStore = msgStore
                        , chanSameSignalTree = \_ _ -> True
                        , chanToSignalTree = tst
                        , chanMaxHistory = maxHist
                        }

  return (retChan, newMessage)

type Meta = [Tree ([String], Either String Int)]
data History' = History' Bool (S.Seq (Double, Vector Double)) Meta

-- History channel which does NOT automatically generates the signal tree for you.
-- This is the internal part which should be wrapped by addHistoryChannel'.
newHistoryChannel' ::
  String -> IO (Channel History', Double -> Vector Double -> Maybe Meta -> IO ())
newHistoryChannel' name = do
  maxHist <- IORef.newIORef 200

  msgStore <- Gtk.listStoreNew []

  let newMessage :: Double -> Vector Double -> Maybe Meta -> IO ()
      newMessage nextTime nextVal maybeMeta = do
        Gtk.postGUIAsync $ do
          let val = (nextTime, nextVal)
          size <- Gtk.listStoreGetSize msgStore
          if size == 0
            then case maybeMeta of
                   Just meta -> Gtk.listStorePrepend msgStore (History' True (S.singleton val) meta)
                   Nothing -> error $ unlines
                              [ "error: History channel has size 0 message store but no reset."
                              , "This means that the first message the plotter saw didn't contain the meta-data."
                              , "This was probably caused by starting the plotter AFTER sending the first telemetry message."
                              ]
            else do History' _ vals0 meta <- Gtk.listStoreGetValue msgStore 0
                    maxHistory <- IORef.readIORef maxHist
                    let dropped = S.drop (1 + S.length vals0 - maxHistory) (vals0 S.|> val)
                    Gtk.listStoreSetValue msgStore 0 (History' False dropped meta)

          -- reset on new meta
          case maybeMeta of
            Nothing -> return () -- no reset
            Just meta -> Gtk.listStoreSetValue msgStore 0 (History' True (S.singleton val) meta)

  let toSignalTree :: History'
                      -> [Tree ( [String]
                               , Either String (History' -> [[(Double, Double)]])
                               )]
      toSignalTree (History' _ _ meta) = map (fmap f) meta
        where
          f :: ([String], Either String Int)
               -> ([String], Either String (History' -> [[(Double, Double)]]))
          f (n0, Left n1) = (n0, Left n1)
          f (n0, Right k) = (n0, Right g)
            where
              g :: History' -> [[(Double, Double)]]
              g (History' _ vals _) = [map toVal (F.toList vals)]
                where
                  toVal (t, x) = (t, x V.! k)

      sameSignalTree :: History' -> History' -> Bool
      -- assume the signal trees are the same if it's not a reset
      sameSignalTree (History' _ _ _) (History' False _ _) = True
      -- if it's a reset, then compare the signal trees
      sameSignalTree (History' _ _ old) (History' True _ new) = old == new

  let retChan = Channel { chanName = name
                        , chanMsgStore = msgStore
                        , chanSameSignalTree = sameSignalTree
                        , chanToSignalTree = toSignalTree
                        , chanMaxHistory = maxHist
                        }

  return (retChan, newMessage)

-- the list of channels
newChannelWidget :: PlotterOptions -> Channel a -> CC.MVar [Gtk.Window] -> IO Gtk.VBox
newChannelWidget plotterOptions channel graphWindowsToBeKilled = do
  vbox <- Gtk.vBoxNew False 4

  nameBox' <- Gtk.hBoxNew False 4
  nameBox <- labeledWidget (chanName channel) nameBox'

  buttonsBox <- Gtk.hBoxNew False 4

  -- button to clear history
  buttonAlsoDoNothing <- Gtk.buttonNewWithLabel "also do nothing"
--  void $ Gtk.onClicked buttonAlsoDoNothing $ do
--    putStrLn "i promise, nothing happens"
--    -- CC.modifyMVar_ logData (const (return S.empty))
--    return ()
  let triggerYo action = on buttonAlsoDoNothing Gtk.buttonActivated action >> return ()

  -- button to make a new graph
  buttonNew <- Gtk.buttonNewWithLabel "new graph"
  void $ on buttonNew Gtk.buttonActivated $ do
    graphWin <-
      newGraph plotterOptions triggerYo (chanName channel)
      (chanSameSignalTree channel)
      (chanToSignalTree channel)
      (chanMsgStore channel)

    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))


  -- entry to set history length
  maxHistoryEntryAndLabel <- Gtk.hBoxNew False 4
  maxHistoryLabel <- Gtk.vBoxNew False 4 >>= labeledWidget "max history:"
  maxHistoryEntry <- Gtk.entryNew
  Gtk.set maxHistoryEntry
    [ Gtk.entryEditable := True
    , Gtk.widgetSensitive := True
    ]
  Gtk.entrySetText maxHistoryEntry "500"
  let updateMaxHistory = do
        txt <- Gtk.get maxHistoryEntry Gtk.entryText
        let reset = Gtk.entrySetText maxHistoryEntry "(max)"
        case readMaybe txt :: Maybe Int of
          Nothing ->
            putStrLn ("max history: couldn't make an Int out of \"" ++ show txt ++ "\"") >> reset
          Just 0  -> putStrLn ("max history: must be greater than 0") >> reset
          Just k  -> IORef.writeIORef (chanMaxHistory channel) k

  void $ on maxHistoryEntry Gtk.entryActivate updateMaxHistory
  updateMaxHistory


  Gtk.set maxHistoryEntryAndLabel
    [ Gtk.containerChild := maxHistoryLabel
    , Gtk.boxChildPacking maxHistoryLabel := Gtk.PackNatural
    , Gtk.containerChild := maxHistoryEntry
    , Gtk.boxChildPacking maxHistoryEntry := Gtk.PackNatural
    ]


  -- put all the buttons/entries together
  Gtk.set buttonsBox
    [ Gtk.containerChild := buttonNew
    , Gtk.boxChildPacking buttonNew := Gtk.PackNatural
    , Gtk.containerChild := buttonAlsoDoNothing
    , Gtk.boxChildPacking buttonAlsoDoNothing := Gtk.PackNatural
    , Gtk.containerChild := maxHistoryEntryAndLabel
    , Gtk.boxChildPacking maxHistoryEntryAndLabel := Gtk.PackNatural
    ]

  Gtk.set vbox
    [ Gtk.containerChild := nameBox
    , Gtk.boxChildPacking   nameBox := Gtk.PackNatural
    , Gtk.containerChild := buttonsBox
    , Gtk.boxChildPacking   buttonsBox := Gtk.PackNatural
    ]

  return vbox


----  -- save all channel data when this button is pressed
----  void $ on renderer3 Gtk.cellToggled $ \pathStr -> do
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
----    void $ CC.forkIO writerThread
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
