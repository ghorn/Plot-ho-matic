{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.HistoryChannel
       ( XAxisType(..)
       , newHistoryChannel
       , newHistoryChannel'
       ) where

import Control.Lens ( (^.) )
import Control.Monad ( when )
import qualified Data.Foldable as F
import qualified Data.IORef as IORef
import Data.Time ( NominalDiffTime, getCurrentTime, diffUTCTime )
import Data.Tree ( Tree )
import qualified Data.Tree as Tree
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import qualified Data.Sequence as S

import Accessors

import PlotHo.Channels
import PlotHo.PlotTypes ( Channel(..) )

data History a = History (S.Seq (a, Int, NominalDiffTime))
type HistorySignalTree a = Tree.Forest ([String], Either String (History a -> [[(Double, Double)]]))
data History' = History' Bool (S.Seq (Double, Vector Double)) Meta

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

-- | Simplified time-series channel which automatically generates the signal tree
-- based on 'Accessors.Lookup'.
-- You have to recompile the plotter if the types change.
-- The plotter will plot a time series of messages put by the action returned by this function.
-- The worker should pass True to reset the message history, so sending True the first message and False subsequent messages is a good starting place.
-- If this is too restrictive, use the more generic 'newChannel'
-- and use a Tree-like type to represent your data, or use 'newHistoryChannel''.
newHistoryChannel ::
  forall a
  . Lookup a
  => String -- ^ channel name
  -> XAxisType -- ^ what to use for the X axis
  -> IO (Channel, a -> Bool -> IO ()) -- ^ return a channel and a "new message" action which can also reset the history
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


-- | History channel which supports data whose structure can change.
-- It does NOT automatically generate the signal tree like 'newHistoryChannel' does.
-- This returns a channel and an action which takes x-axis value, a vector of y axis values, and a tree which
-- indexes these y axis values.
-- If the data structure changes, a new tree should be sent, otherwise there could be indexing errors.
newHistoryChannel' ::
  String -- ^ channel name
  -> IO (Channel, Double -> Vector Double -> Maybe Meta -> IO ())
newHistoryChannel' name = do
  maxHist <- IORef.newIORef 500

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
