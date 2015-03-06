{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import qualified Control.Concurrent as CC
import GHC.Generics ( Generic )
--import qualified System.Remote.Monitoring as EKG

import PlotHo ( Lookup, XAxisType(..), runPlotter, addHistoryChannel )

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

axyz0 :: Axyz
axyz0 = MkAxyz
        7
--        (S.fromList [MkXyz 1 2 3])
        (MkXyz 1 2 3)

xyz0 :: Xyz
xyz0 = MkXyz 1 2 3

incrementAxyz :: Axyz -> Axyz
incrementAxyz (MkAxyz a _) = MkAxyz
                             (a+0.2)
--                             (S.take 5 (xyz <| xyzs))
                             xyz'
  where
    xyz' = MkXyz (sin a) (cos a) (sin a * cos a)

incrementXyz :: Xyz -> Xyz
incrementXyz (MkXyz a _ _) = MkXyz (a+0.3) (2 * sin a) (3 * cos a)

-- a random function to write a bunch of data to a chan
channelWriter :: Int -> Int -> (a -> a) -> a -> (a -> Bool -> IO ()) -> IO ()
channelWriter count delay increment x' chan = do
  --putStrLn $ "writing: " ++ show count
  CC.threadDelay delay
  chan (increment x') False
  channelWriter (count + 1) delay increment (increment x') chan

main :: IO ()
main = do
--  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000

  runPlotter $ do
    addHistoryChannel "posPlos"  XAxisTime   $ channelWriter 0 50000 incrementAxyz axyz0
    addHistoryChannel "pos"      XAxisCount  $ channelWriter 0 60000 incrementXyz xyz0
    addHistoryChannel "posPlos"  XAxisTime0  $ channelWriter 0 50000 incrementAxyz axyz0
    addHistoryChannel "pos"      XAxisCount0 $ channelWriter 0 60000 incrementXyz xyz0
