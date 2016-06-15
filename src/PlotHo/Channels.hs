{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.Channels
       ( newChannel
       ) where

import qualified Data.IORef as IORef
import Data.Tree ( Tree )
import qualified "gtk3" Graphics.UI.Gtk as Gtk

import PlotHo.PlotTypes ( Channel(..) )

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
