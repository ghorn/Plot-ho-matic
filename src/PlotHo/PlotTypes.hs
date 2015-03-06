{-# OPTIONS_GHC -Wall #-}
--{-# Language ExistentialQuantification #-}
--{-# Language GADTs #-}

module PlotHo.PlotTypes
       ( ListViewInfo(..)
       , SignalTree
       , Getter
       ) where

import Data.Time ( NominalDiffTime )
import qualified Data.Sequence as S
import qualified Data.Tree as Tree

type Getter a = Either (a -> Double) (S.Seq (a, Int, NominalDiffTime) -> [[(Double,Double)]])

-- | a tree of name/getter pairs
type SignalTree a = Tree.Forest (String, String, Maybe (Getter a))

data ListViewInfo a = ListViewInfo { lviName :: String
                                   , lviType :: String
                                   , lviGetter :: Maybe (Getter a)
                                   , lviMarked :: Bool
                                   }
