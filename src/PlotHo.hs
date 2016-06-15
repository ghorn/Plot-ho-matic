{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}

module PlotHo
       ( -- * Usage
         -- $simple

         -- ** Dynamic data
         -- $dynamic

         Plotter
       , runPlotter
       , XAxisType(..)
       , addHistoryChannel
       , Meta
       , addHistoryChannel'
       , addChannel
         -- * re-exported for convenience
       , Lookup
       ) where

import Accessors ( Lookup )

import PlotHo.Channels ( Meta, addChannel )
import PlotHo.HistoryChannel ( XAxisType(..), addHistoryChannel, addHistoryChannel' )
import PlotHo.Plotter ( Plotter, runPlotter )

-- $simple
--
-- The easiest way to use this library is to use 'GHC.Generics.Generic' to derive an instance of 'Accessors.Lookup' for your data type, and then use 'addHistoryChannel' to create a time series.
-- You need to pass 'addHistoryChannel' an action which periodically sends a new message.
--
-- For example:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import GHC.Generics ( Generic )
-- > import Accessors ( Lookup )
-- > import PlotHo
-- >
-- > data Foo =
-- >   = Foo
-- >     { value1 :: Double
-- >     , value2 :: Double
-- >     } deriving Generic
-- > instance Lookup Foo
-- >
-- > messageSender :: (Foo -> True -> IO ()) -> IO ()
-- > messageSender newMessage = forever $ do
-- >   CC.threadDelay 100000
-- >   foo <- receiveFooFromNetworkOrSomething :: IO Foo
-- >   let reset = False -- never reset in this example
-- >   newMessage foo reset
-- >
-- > main :: IO ()
-- > main = runPlotter $
-- >   addHistoryChannel "it's foo" XAxisCount messageSender
--
-- When the plotter executes, @messageSender@ will be forked and will
-- forever listen for new messages. Every time it receives a new message
-- it will call the @newMessage@ action which instructs the plotter to
-- add a data point and redraw.


-- $dynamic
--
-- (Placeholder)
