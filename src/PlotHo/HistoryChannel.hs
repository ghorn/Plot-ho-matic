{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}

module PlotHo.HistoryChannel
       ( Meta
       , XAxisType(..)
       , newHistoryChannel
       , newHistoryChannel'
       ) where

import qualified Control.Concurrent as CC
import Control.Lens ( (^.) )
import Control.Monad ( when )
import qualified Data.Foldable as F
import qualified Data.IORef as IORef
import Data.Time ( NominalDiffTime, getCurrentTime, diffUTCTime )
import Data.Tree ( Tree )
import qualified Data.Tree as Tree
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Sequence as S

import Accessors

import PlotHo.Channel ( newChannel' )
import PlotHo.PlotTypes ( Channel(..), Channel'(..), SignalTree, debug )

type Meta = Tree ([String], Either String Int)

newtype History a = History (S.Seq (a, Int, NominalDiffTime))
data History' = History' !Bool !(S.Seq (Double, Vector Double)) !Meta

data XAxisType =
  XAxisTime -- ^ time since the first message
  | XAxisTime0 -- ^ time since the first message, normalized to 0 (to reduce plot jitter)
  | XAxisCount -- ^ message index
  | XAxisCount0 -- ^ message index, normalized to 0 (to reduce plot jitter)

historySignalTree :: forall a . Lookup a => XAxisType -> String -> SignalTree (History a)
historySignalTree axisType topName = makeSignalTree' [topName] accessors
  where
    makeSignalTree' :: [String] -> AccessorTree a -> SignalTree (History a)
    makeSignalTree' myFieldName (Right (GAData _ (GAConstructor cname children))) =
      Tree.Node
      (reverse myFieldName, Left cname)
      (map (\(getterName, child) -> makeSignalTree' (fromMName getterName:myFieldName) child) children)

    makeSignalTree' myFieldName (Right (GAData _ (GASum enum))) =
      Tree.Node (reverse myFieldName, Right (toHistoryGetter (fromIntegral . eToIndex enum))) []
    makeSignalTree' myFieldName (Left field) =
      Tree.Node (reverse myFieldName, Right (toHistoryGetter (toDoubleGetter field))) []

    fromMName (Just x) = x
    fromMName Nothing = "()"

    toDoubleGetter :: GAField a -> (a -> Double)
    toDoubleGetter (FieldDouble f) = (^. f)
    toDoubleGetter (FieldFloat f) = realToFrac . (^. f)
    toDoubleGetter (FieldInt8 f) = fromIntegral . (^. f)
    toDoubleGetter (FieldInt16 f) = fromIntegral . (^. f)
    toDoubleGetter (FieldInt32 f) = fromIntegral . (^. f)
    toDoubleGetter (FieldInt64 f) = fromIntegral . (^. f)
    toDoubleGetter (FieldWord8 f) = fromIntegral . (^. f)
    toDoubleGetter (FieldWord16 f) = fromIntegral . (^. f)
    toDoubleGetter (FieldWord32 f) = fromIntegral . (^. f)
    toDoubleGetter (FieldWord64 f) = fromIntegral . (^. f)
    toDoubleGetter (FieldBool _) = const 0
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
  let toSignalTree :: History a -> SignalTree (History a)
      toSignalTree = const (historySignalTree xaxisType name)

      sameSignalTree _ _ = True

  (channel', newHistoryMessage) <- newChannel' name sameSignalTree toSignalTree
    :: IO (Channel' (History a), History a -> IO ())

  -- Put the first message immediately in order to immediately build the signal tree
  -- so that the user can see it without waiting for the first message.
  newHistoryMessage (History mempty)

  let newMessage :: a -> Bool -> IO ()
      newMessage next reset = do
        debug "newMessage(newHistoryChannel): message received"
        -- grab the time and counter
        time <- getCurrentTime
        when reset $ do
          IORef.writeIORef time0 time
          IORef.writeIORef counter 0

        k <- IORef.readIORef counter
        time0' <- IORef.readIORef time0

        IORef.writeIORef counter (k+1)
        let val = (next, k, diffUTCTime time time0')

        latestChanValue <- CC.readMVar (chanLatestValueMVar channel')
        oldTimeSeries <- case latestChanValue of
          Just (History r, _) -> return r
          Nothing -> error "newMessage(newHistoryChannel): the 'impossible' happened: channel has no latest message"

        maxHistory <- IORef.readIORef (chanMaxHistory channel')
        let newTimeSeries
              | reset = S.singleton val
              | otherwise = S.drop (1 + S.length oldTimeSeries - maxHistory) (oldTimeSeries S.|> val)
        debug "newMessage(newHistoryChannel): new history message calling internal newMessage"
        newHistoryMessage (History newTimeSeries)

      clearHistory :: History a -> History a
      clearHistory = const (History mempty)

  return (Channel (channel' {chanClearHistory = Just clearHistory}) , newMessage)


-- | History channel which supports data whose structure can change.
-- It does NOT automatically generate the signal tree like 'newHistoryChannel' does.
-- This returns a channel and an action which takes x-axis value, a vector of y axis values, and a tree which
-- indexes these y axis values.
-- If the data structure changes, a new tree should be sent, otherwise there could be indexing errors.
newHistoryChannel' ::
  String -- ^ channel name
  -> IO (Channel, Either Meta (Double, Vector Double) -> IO ())
newHistoryChannel' name = do
  let toSignalTree :: History'
                      -> Tree ( [String]
                              , Either String (History' -> [[(Double, Double)]])
                              )
      toSignalTree (History' _ _ meta) = fmap f meta
        where
          f :: ([String], Either String Int) -> ([String], Either String (History' -> [[(Double, Double)]]))
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

  (channel', newHistoryMessage) <- newChannel' name sameSignalTree toSignalTree
    :: IO (Channel' History', History' -> IO ())

  let newMessage :: Either Meta (Double, Vector Double) -> IO ()
      newMessage msg = do
        latestChannelValue <- CC.readMVar (chanLatestValueMVar channel')
        case (latestChannelValue, msg) of
          -- first message and no meta to go with it
          (Nothing, Right _) ->
            putStr $ unlines
              [ "WARNING: First message seen by Plot-ho-matic doesn't have signal tree meta-data."
              , "This was probably caused by starting the plotter AFTER sending the first telemetry message."
              , "Try restarting the application sending messages."
              ]
          -- any message with meta is a reset
          (_, Left meta) -> newHistoryMessage (History' True mempty meta)
          -- later message without meta - no reset
          (Just (History' _ oldTimeSeries meta, _), Right (nextTime, nextVal)) -> do
            maxHistory <- IORef.readIORef (chanMaxHistory channel')
            let newTimeSeries =
                  S.drop (1 + S.length oldTimeSeries - maxHistory) (oldTimeSeries S.|> (nextTime, nextVal))
            newHistoryMessage (History' False newTimeSeries meta)

      clearHistory :: History' -> History'
      clearHistory (History' reset _ meta) = History' reset mempty meta

  return (Channel (channel' {chanClearHistory = Just clearHistory}) , newMessage)
