{-# OPTIONS_GHC -Wall #-}
--{-# Language ExistentialQuantification #-}
--{-# Language GADTs #-}
{-# Language PackageImports #-}

module PlotHo.PlotTypes
       ( Channel(..)
       , GraphInfo(..)
       , ListViewInfo(..)
       , AxisScaling(..)
       , MarkedState(..)
       ) where

import Data.Tree ( Tree )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import Data.IORef ( IORef )

data MarkedState =
  On | Off | Inconsistent deriving (Eq, Show)

data ListViewInfo a =
  ListViewInfo
  { lviName :: [String]
  , lviTypeOrGetter :: Either String (a -> [[(Double,Double)]])
  , lviMarked :: MarkedState
  }

instance Show (ListViewInfo a) where
  show (ListViewInfo n (Left t) m)  = "ListViewInfo " ++ show (n,t,m)
  show (ListViewInfo n (Right _) m) = "ListViewInfo " ++ show (n,m)

data AxisScaling = LogScaling
                 | LinearScaling

-- what the graph should draw
data GraphInfo a =
  GraphInfo { giXScaling :: AxisScaling
            , giYScaling :: AxisScaling
            , giXRange :: Maybe (Double,Double)
            , giYRange :: Maybe (Double,Double)
            , giGetters :: [(String, a -> [[(Double,Double)]])]
            , giTitle :: Maybe String
            }

data Channel a =
  Channel { chanName :: String
          , chanMsgStore :: Gtk.ListStore a
          , chanSameSignalTree :: a -> a -> Bool
          , chanToSignalTree :: a -> [Tree ( [String]
                                           , Either String (a -> [[(Double, Double)]])
                                           )]
          , chanMaxHistory :: IORef Int
          }
