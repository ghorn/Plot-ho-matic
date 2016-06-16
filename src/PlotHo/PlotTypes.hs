{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# Language TemplateHaskell #-}

module PlotHo.PlotTypes
       ( AxisType(..)
       , Axes(..)
       , Channel(..)
       , Channel'(..)
       , GraphComms(..)
       , GraphInfo(..)
       , ListViewInfo(..)
       , MarkedState(..)
       , PlotterOptions(..)
       , SignalTree
       , XY(..)
       , debug
       , defaultHistoryRange
       , xaxis, yaxis
       ) where

import qualified Control.Concurrent as CC
import Control.Lens
import Control.Monad.IO.Class ( MonadIO ) -- , liftIO )
import Data.Tree ( Tree )
import Data.IORef ( IORef )
import qualified Data.Map.Strict as M
import qualified "gtk3" Graphics.UI.Gtk as Gtk

debug :: MonadIO m => String -> m ()
--debug = liftIO . putStrLn
debug = const (return ())

data MarkedState =
  On | Off | Inconsistent deriving (Eq, Show)

data ListViewInfo a =
  ListViewInfo
  { lviName :: ![String]
  , lviTypeOrGetter :: !(Either String (a -> [[(Double,Double)]]))
  , lviMarked :: !MarkedState
  }

instance Show (ListViewInfo a) where
  show (ListViewInfo n (Left t) m)  = "ListViewInfo " ++ show (n,t,m)
  show (ListViewInfo n (Right _) m) = "ListViewInfo " ++ show (n,m)

type SignalTree a = [Tree ( [String]
                          , Either String (a -> [[(Double, Double)]])
                          )]

data AxisType
  = LogScaling
  | LinearScalingAutoRange
  | LinearScalingHistoryRange
  | LinearScalingManualRange

defaultHistoryRange :: (Double, Double)
defaultHistoryRange = (read "Infinity", - read "Infinity")

data XY a
  = XY
    { _xaxis :: !a
    , _yaxis :: !a
    }
makeLenses ''XY

data Axes a
  = Axes
    { axesType :: !(XY AxisType)
    , axesManualRange :: !(XY (a, a))
    }

-- what the graph should draw
data GraphInfo a
  = GraphInfo
    { giGetters :: ![(String, a -> [[(Double,Double)]])]
    , giTitle :: !(Maybe String)
    }

data GraphComms a
  = GraphComms
    { gcRedrawSignal :: IO ()
    , gcMsgStore :: CC.MVar (Maybe (a, Maybe (SignalTree a)))
      -- ^ The MVar is always full, we use Maybe to decide if a first message has been received.
      -- If the signal tree is Just, a rebuild is needed.
    }

data Channel where
  Channel :: Channel' a -> Channel

data Channel' a
  = Channel'
    { chanName :: String
    , chanSameSignalTree :: a -> a -> Bool
    , chanToSignalTree :: a -> SignalTree a
    , chanMaxHistory :: IORef Int
    , chanClearHistory :: Maybe (a -> a)
    , chanGraphCommsMap :: CC.MVar (M.Map Gtk.Window (GraphComms a))
    , chanLatestValueMVar :: CC.MVar (Maybe (a, SignalTree a))
    }

-- | Some options
data PlotterOptions
  = PlotterOptions
    { maxDrawRate :: Double -- ^ limit the draw frequency to this number in Hz
    }
