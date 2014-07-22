{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -ddump-splices #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveGeneric #-}

module Main ( main ) where

import qualified Control.Concurrent as CC
import qualified Data.Sequence as S
import Data.Serialize ( Serialize )
import Data.Sequence ( (<|) )
import GHC.Generics ( Generic )
import qualified Data.Tree as Tree
--import qualified System.Remote.Monitoring as EKG

import Plotter --( Lookup, SignalTree, runPlotter, newChannel, makeSignalTree )

data Xyz = MkXyz { x :: Double
                 , y :: Double
                 , z :: Double
                 } deriving Generic
data Axyz = MkAxyz { lol :: Double
--                   , xyzList :: S.Seq Xyz
                   , xyz :: Xyz
                   } deriving Generic
instance Lookup Xyz
instance Lookup Axyz
instance Serialize Xyz
instance Serialize Axyz

axyz0 = MkAxyz
        7
--        (S.fromList [MkXyz 1 2 3])
        (MkXyz 1 2 3)

incrementAxyz :: Axyz -> Axyz
incrementAxyz (MkAxyz a _) = MkAxyz
                             (a+0.2)
--                             (S.take 5 (xyz <| xyzs))
                             xyz
  where
    xyz = MkXyz (sin a) (cos a) (sin a * cos a)

incrementXyz :: Xyz -> Xyz
incrementXyz (MkXyz a _ _) = MkXyz (a+0.3) (2 * sin a) (3 * cos a)

-- a random function to write a bunch of data to a chan
channelWriter :: Int -> (a -> a) -> (a -> IO ()) -> a -> IO ()
channelWriter delay increment chan x = do
  CC.threadDelay delay
  chan (increment x)
  channelWriter delay increment chan (increment x)

st :: SignalTree Axyz
st = makeSignalTree axyz0

woo :: (String, String, Maybe a) -> String
woo (x,y,z) = show (x,y, fmap (const ()) z)

main :: IO ()
main = do
--  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000
  putStrLn $ Tree.drawForest (map (fmap woo) st)

  (c0, chan0, _) <- newChannel "posPlus" st
--  (c1, chan1) <- newChannel "pos"     (makeSignalTree ''Xyz)

  producerTid0 <- CC.forkIO $ channelWriter 50000 incrementAxyz chan0 axyz0
--  producerTid1 <- CC.forkIO $ channelWriter 60000 incrementXyz chan1 $ MkXyz 0 2 3

  runPlotter c0
--             ,c1
             
    [ producerTid0
--  , producerTid1
--  , ekgTid
    ]
