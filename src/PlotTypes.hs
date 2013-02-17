{-# OPTIONS_GHC -Wall #-}
{-# Language ExistentialQuantification #-}

module PlotTypes ( Channel(..)
                 , PbPrim(..)
                 , PbTree(..)
                 , XAxisType(..)
                 , pbpToFrac
                 , pbTreeToTree
                 ) where

import Control.Concurrent ( MVar )
import qualified Data.ByteString.Lazy as BSL
import Data.Sequence ( Seq )
import Data.Time ( NominalDiffTime )
import Data.Tree ( Tree(..) )
import qualified Text.ProtocolBuffers.Header as P'

data XAxisType a = XAxisTime
                 | XAxisCounter
                 | XAxisFun (String, a -> PbPrim)

data Channel = forall a. Channel { chanName :: String
                                 , chanGetters :: Tree (String, Maybe (a -> PbPrim))
                                 , chanSeq :: MVar (Seq (a,Int,NominalDiffTime))
                                 , chanMaxHist :: MVar Int
                                 }

data PbTree a = PbfGetter (a -> PbPrim)
              | PbfStruct [(String,PbTree a)]
              | forall b. PbfSeq   (a -> Seq   b) (PbTree b)
              | forall b. PbfMaybe (a -> Maybe b) (PbTree b)


pbTreeToTree :: String -> PbTree a -> Tree (String, Maybe (a -> PbPrim))
pbTreeToTree name (PbfGetter get) = Node (name, Just get) []
pbTreeToTree name (PbfStruct stuff) =
  Node (name, Nothing) (map (uncurry pbTreeToTree) stuff)

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

pbpToFrac :: Fractional a => PbPrim -> Maybe a
pbpToFrac (PbDouble c)     = Just $ realToFrac c
pbpToFrac (PbFloat c)      = Just $ realToFrac c
pbpToFrac (PbInt32 c)      = Just $ realToFrac c
pbpToFrac (PbInt64 c)      = Just $ realToFrac c
pbpToFrac (PbWord32 c)     = Just $ realToFrac c
pbpToFrac (PbWord64 c)     = Just $ realToFrac c
pbpToFrac (PbBool c)       = Just $ (\x -> if x then 1 else 0) c
pbpToFrac (PbUtf8 _)       = Nothing
pbpToFrac (PbByteString _) = Nothing
