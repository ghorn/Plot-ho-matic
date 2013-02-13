{-# OPTIONS_GHC -Wall #-}

module PlotTypes ( GraphInfo(..)
                 , Channel(..)
                 , PbPrim(..)
                 , pbpToFrac
                 ) where

import Control.Concurrent ( MVar )
import Data.Sequence ( Seq )
import qualified Data.ByteString.Lazy as BSL
import qualified Text.ProtocolBuffers.Header as P'

-- what the graph should draw
data GraphInfo a = GraphInfo (MVar (Seq a)) (MVar Int) [(String, a -> PbPrim)]

data Channel a = Channel { chanGetters :: [(String, a -> PbPrim)]
                         , chanSeq :: MVar (Seq a)
                         , chanMaxNum :: MVar Int
                         }

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

pbpToFrac :: Fractional a => PbPrim -> a
pbpToFrac (PbDouble c)     = realToFrac c
pbpToFrac (PbFloat c)      = realToFrac c
pbpToFrac (PbInt32 c)      = realToFrac c
pbpToFrac (PbInt64 c)      = realToFrac c
pbpToFrac (PbWord32 c)     = realToFrac c
pbpToFrac (PbWord64 c)     = realToFrac c
pbpToFrac (PbBool c)       = (\x -> if x then 1 else 0) c
--pbpToFrac (PbUtf8 _)       = Nothing
--pbpToFrac (PbByteString _) = Nothing
