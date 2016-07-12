{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.PlotTypes
       ( AxisType(..)
       , Axes(..)
       , Channel(..)
       , Channel'(..)
       , Element(..)
       , Element'(..)
       , GraphComms(..)
       , ListViewInfo(..)
       , MarkedState(..)
       , PlotterOptions(..)
       , SignalTree
       , XY(..)
       , debug
       , defaultHistoryRange
       ) where

import qualified Control.Concurrent as CC
import Control.Monad.IO.Class ( MonadIO ) -- , liftIO )
import Data.Default.Class ( Default(..) )
import Data.IORef ( IORef )
import qualified Data.Map.Strict as M
import Data.Tree ( Tree )
import qualified "gtk3" Graphics.UI.Gtk as Gtk

debug :: MonadIO m => String -> m ()
--debug = liftIO . putStrLn
debug = const (return ())

data MarkedState =
  On | Off | Inconsistent deriving (Eq, Show)

data Element where
  Element :: Element' a -> Element

data Element' a
  = Element'
    { eChannel :: Channel' a
    , eMsgStore :: CC.MVar (Maybe (a, Maybe (SignalTree a)))
    , ePlotValueRef :: IORef a
    , eIndex :: Int
    }

data ListViewInfo where
  ListViewInfo ::
    { lviName :: ![String]
    , lviTypeOrGetter :: !(Either String (a -> [[(Double,Double)]]))
    , lviMarked :: !MarkedState
    , lviPlotValueRef :: IORef a
    } -> ListViewInfo

instance Show ListViewInfo where
  show (ListViewInfo n (Left t) m _)  = "ListViewInfo " ++ show (n,t,m)
  show (ListViewInfo n (Right _) m _) = "ListViewInfo " ++ show (n,m)

type SignalTree a =
  Tree ( [String]
       , Either String (a -> [[(Double, Double)]])
       )

data AxisType
  = LinearScalingAutoRange
  | LinearScalingHistoryRange
  | LinearScalingManualRange
  | LogScaling
  deriving (Enum)

defaultHistoryRange :: (Double, Double)
defaultHistoryRange = (read "Infinity", - read "Infinity")

data XY a
  = XY
    { xaxis :: !a
    , yaxis :: !a
    }

data Axes a
  = Axes
    { axesType :: !(XY AxisType)
    , axesManualRange :: !(XY (a, a))
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
    , defaultXAxis :: AxisType
    , defaultYAxis :: AxisType
    }

instance Default PlotterOptions where
  def =
    PlotterOptions
    { maxDrawRate = 40
    , defaultXAxis = LinearScalingAutoRange
    , defaultYAxis = LinearScalingAutoRange
    }
