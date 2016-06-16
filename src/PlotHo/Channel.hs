{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module PlotHo.Channel
       ( newChannel, newChannel'
       ) where

import qualified Control.Concurrent as CC
import qualified Data.IORef as IORef
import Data.Tree ( Tree )

import PlotHo.PlotTypes ( Channel(..), Channel'(..), GraphComms(..), debug )

-- | This is the general interface to plot whatever you want.
-- Use this when you want to give the whole time series in one go, rather than one at a time
-- such as with 'addHistoryChannel'.
-- Using types or data, you must encode the signal tree with the message so that
-- the plotter can build you the nice signal tree.
newChannel ::
  forall a
  . String -- ^ channel name
  -> (a -> a -> Bool) -- ^ Is the signal tree the same? This is used for instance if signals have changed and the plotter needs to rebuild the signal tree. This lets you keep the plotter running and change other programs which send messages to the plotter.
  -> (a -> [Tree ([String], Either String (a -> [[(Double, Double)]]))]) -- ^ how to build the signal tree
  -> IO (Channel, a -> IO ()) -- ^ Return a channel and a "new message" function. You should for a thread which receives messages and calls this action.
newChannel name sameSignalTree toSignalTree = do
  (channel', newMessage) <- newChannel' name sameSignalTree toSignalTree
  return (Channel channel', newMessage)

-- | Monomorphic version of 'newChannel'. Must be wrapped in 'Channel' in order to plot.
newChannel' ::
  forall a
  . String
  -> (a -> a -> Bool)
  -> (a -> [Tree ([String], Either String (a -> [[(Double, Double)]]))])
  -> IO (Channel' a, a -> IO ())
newChannel' name sameSignalTree toSignalTree = do
  lastMsgMVar <- CC.newMVar Nothing
  graphCommsMapMVar <- CC.newMVar mempty
  maxHist <- IORef.newIORef 0

  let newMessage :: a -> IO ()
      newMessage newVal = do
        debug "newMessage(newChannel): got message"

        -- If it's the first message or if the signal tree has changed, return a signal tree.
        -- If it's a later message and the signal tree is unchanged, return Nothing.
        mlastMsg <- CC.takeMVar lastMsgMVar
        let (latestSignalTree, signalTreeNewOrChanged) = case mlastMsg of
              -- Not the first message.
              Just (oldVal, oldSignalTree)
                -- Signal tree is unchanged.
                | sameSignalTree oldVal newVal -> (oldSignalTree, False)
                  -- Signal tree has changed.
                | otherwise -> (toSignalTree newVal, True)
              -- First message. Always build signal tree.
              Nothing -> (toSignalTree newVal, True)
        CC.putMVar lastMsgMVar (Just (newVal, latestSignalTree))
        -- Be careful not to keep a reference to oldVal around so that we don't build up a chain
        -- of references to all the old values.
        -- Evaluating signalTreeNewOrChanged should take care of that.
        signalTreeNewOrChanged `seq` return ()


        -- now send the data to each graph
        let updateGraph :: GraphComms a -> IO ()
            updateGraph (GraphComms {gcRedrawSignal = redraw, gcMsgStore = graphMsgStore}) = do

              mmsgStore <- CC.takeMVar graphMsgStore

              case (mmsgStore, signalTreeNewOrChanged) of
                -- This graph hasn't gotten a message yet, give it the latest.
                (Nothing, _) -> CC.putMVar graphMsgStore (Just (newVal, Just latestSignalTree))
                -- If we have a new signal tree, force it on the graph no matter what.
                (_, True) ->
                  CC.putMVar graphMsgStore (Just (newVal, Just latestSignalTree))
                -- If there is no new signal tree, but the new value and the graph's latest signal tree,
                -- processed or un processed.
                (Just (_, graphsLatestSignalTree), False) ->
                  CC.putMVar graphMsgStore (Just (newVal, graphsLatestSignalTree))

              -- tell the graph it needs to redraw
              debug "newMessage(newChannel): signaling redraw"
              redraw

        -- call any redraw functions needed
        CC.readMVar graphCommsMapMVar >>= mapM_ updateGraph

  let retChan =
        Channel'
        { chanName = name
        , chanLatestValueMVar = lastMsgMVar
        , chanSameSignalTree = sameSignalTree
        , chanToSignalTree = toSignalTree
        , chanMaxHistory = maxHist
        , chanClearHistory = Nothing
        , chanGraphCommsMap = graphCommsMapMVar
        }

  return (retChan, newMessage)
