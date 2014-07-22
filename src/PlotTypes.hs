{-# OPTIONS_GHC -Wall #-}
--{-# Language ExistentialQuantification #-}
--{-# Language GADTs #-}

module PlotTypes
       ( Channel(..)
       , ListViewInfo(..)
       , SignalTree
       , Getter
       ) where

import Control.Concurrent ( MVar, ThreadId )
import Data.Time ( NominalDiffTime )
import qualified Data.Sequence as S
import qualified Graphics.UI.Gtk as Gtk
import qualified Data.Tree as Tree

type Getter a = Either (a -> Double) (S.Seq (a, Int, NominalDiffTime) -> [[(Double,Double)]])

-- | a tree of name/getter pairs
type SignalTree a = Tree.Forest (String, String, Maybe (Getter a))

data ListViewInfo a = ListViewInfo { lviName :: String
                                   , lviType :: String
                                   , lviGetter :: Maybe (Getter a)
                                   , lviMarked :: Bool
                                   }

data Channel a =
  Channel { chanName :: String
          , chanTraj :: MVar (S.Seq (a, Int, NominalDiffTime))
          , chanMaxHist :: MVar Int
          , chanSignalTreeStore :: Gtk.ListStore (SignalTree a)
          , chanServerThreadId :: ThreadId
          }
