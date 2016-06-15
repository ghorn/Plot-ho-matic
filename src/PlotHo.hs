{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}

module PlotHo
       ( -- * Usage
         -- $simple

         -- ** Dynamic data
         -- $dynamic

         runPlotter
       , Channel
       , XAxisType(..)
       , newHistoryChannel
       , Meta
       , newHistoryChannel'
       , newChannel
         -- * re-exported for convenience
       , Lookup
       ) where

import Accessors ( Lookup )

import PlotHo.Channels ( newChannel )
import PlotHo.HistoryChannel ( Meta, XAxisType(..), newHistoryChannel, newHistoryChannel' )
import PlotHo.Plotter ( runPlotter )
import PlotHo.PlotTypes ( Channel )

-- $simple
--
-- The easiest way to use this library is to use 'GHC.Generics.Generic' to derive an instance of 'Accessors.Lookup' for your data type, and then use 'newHistoryChannel' to create a time series plot.
-- The 'newHistoryChannel' function will return an action which is used to send new data to the plotter.
--
-- For example:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import GHC.Generics ( Generic )
-- >
-- > import Accessors ( Lookup )
-- > import Control.Concurrent ( forkIO )
-- > import PlotHo
-- >
-- > data Foo =
-- >   = Foo
-- >     { value1 :: Double
-- >     , value2 :: Double
-- >     } deriving Generic
-- > instance Lookup Foo
-- >
-- > messageSender :: (Foo -> Bool -> IO ()) -> IO ()
-- > messageSender newMessage = go True
-- >   where
-- >     go firstMessage = do
-- >       CC.threadDelay 100000
-- >       foo <- receiveFooFromNetworkOrSomething :: IO Foo
-- >       let reset = firstMessage -- reset on the first message
-- >       newMessage foo reset
-- >       go False
-- >
-- > main :: IO ()
-- > main = do
-- >   (channel, newMessage) <- addHistoryChannel "it's foo" XAxisCount
-- >   _ <- forkIO (messageSender newMessage)
-- >   runPlotter [channel]
--
-- When main is run, a new channel is created which returns the "new message" action.
-- @messageSender@ is then forked and periodically sends new messages to the plotter.
-- The plotter is then started with `runPlotter`.


-- $dynamic
--
-- (Placeholder)
