{-# OPTIONS_GHC -Wall #-}
{-# Language ExistentialQuantification #-}
{-# Language GADTs #-}

module PlotTypes ( Channel(..)
                 , PbPrim(..)
                 , PbTree(..)
                 , PbTree'(..)
                 , XAxisType(..)
                 , pbTreeToTree
                 ) where

import Control.Concurrent ( MVar )
import qualified Data.ByteString.Lazy as BSL
import Data.Sequence ( Seq )
import Data.Time ( NominalDiffTime )
import Data.Tree ( Tree(..) )
import qualified Text.ProtocolBuffers.Header as P'
import Data.Functor.Compose
import Data.Functor.Identity

data XAxisType a = XAxisTime
                 | XAxisCounter
                 | XAxisStaticCounter
                 | XAxisFun (String, a -> PbPrim)

data Channel = forall a. Channel { chanName :: String
                                 , chanGetters :: Tree (String, Maybe (a -> PbPrim))
                                 , chanSeq :: MVar (Seq (a,Int,NominalDiffTime))
                                 , chanMaxHist :: MVar Int
                                 }

data PbTree a where
  PbtGetter :: (a -> PbPrim) -> PbTree a
  PbtStruct :: [(String,PbTree a)] -> PbTree a
  PbtFunctor :: Functor g => (g PbPrim -> PbPrim) -> (a -> g b) -> PbTree b -> PbTree a

data PbTree' a = PbtGetter' (a -> PbPrim)
               | PbtStruct' [(String,PbTree' a)]
               | PbtFunctor' (PbTree' a)


pbTreeToTree :: String -> PbTree a -> Tree (String, Maybe (a -> PbPrim))
pbTreeToTree name tree = pbTreeToTree' name (please tree)

pbTreeToTree' :: String -> PbTree' a -> Tree (String, Maybe (a -> PbPrim))
pbTreeToTree' name (PbtGetter' get) = Node (name, Just get) []
pbTreeToTree' name (PbtStruct' stuff) = Node (name, Nothing) (map (uncurry pbTreeToTree') stuff)
pbTreeToTree' name (PbtFunctor' tree) = pbTreeToTree' ("["++name++"]") tree

please :: PbTree a -> PbTree' a
please = f Identity runIdentity

f :: Functor f => (a -> f b) -> (f PbPrim -> PbPrim) -> PbTree b -> PbTree' a
f afb unfunct (PbtGetter get) = PbtGetter' $ \a -> unfunct $ fmap get (afb a)
f afb unfunct (PbtStruct stuff) = PbtStruct' $ zip names (map (f afb unfunct) trees)
  where
    (names,trees) = unzip stuff
f afb unfunct (PbtFunctor unfunct' h theRest) = PbtFunctor' $ f wow blah theRest
  where
    wow = \a -> Compose (fmap h (afb a))
    blah = \fga -> unfunct $ fmap unfunct' (getCompose fga)

data PbPrim = PbDouble Double
            | PbFloat Float
            | PbInt32 P'.Int32
            | PbInt64 P'.Int64
            | PbWord32 P'.Word32
            | PbWord64 P'.Word64
            | PbBool Bool
            | PbUtf8 P'.Utf8
--            | PbByteString P'.ByteString
            | PbByteString BSL.ByteString
            | PbSeq (Seq PbPrim)
            | PbMaybe (Maybe PbPrim)
