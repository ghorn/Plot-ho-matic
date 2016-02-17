{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}

module PlotHo
       ( Plotter
       , runPlotter
       , PlotterOptions(..)
       , defaultPlotterOptions
       , XAxisType(..)
       , addHistoryChannel
       , Meta
       , addHistoryChannel'
       , addChannel
         -- * re-exported for convenience
       , Lookup
       ) where

import Accessors ( Lookup )

import PlotHo.Channels ( Meta, XAxisType(..), addHistoryChannel, addHistoryChannel', addChannel )
import PlotHo.Plotter ( Plotter, runPlotter )
import PlotHo.PlotTypes ( PlotterOptions(..) )

defaultPlotterOptions :: PlotterOptions
defaultPlotterOptions =
  PlotterOptions
  { maxDrawRate = 40
  }
