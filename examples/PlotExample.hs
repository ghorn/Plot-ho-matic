{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import qualified Control.Concurrent as CC
import GHC.Generics ( Generic )
--import qualified System.Remote.Monitoring as EKG

import PlotHo ( Lookup, XAxisType(..), runPlotter, addHistoryChannel )

data Xyz a = MkXyz { x :: Double
                   , y :: Double
                   , z :: Double
                   , zz :: a
                   } deriving Generic
data Axyz = MkAxyz { lol :: Double
--                   , xyzList :: S.Seq Xyz
                   , xyz1 :: Xyz (Xyz (Xyz Double))
                   , xyz2 :: Xyz Double
                   } deriving Generic
instance Lookup a => Lookup (Xyz a)
instance Lookup Axyz

axyz0 :: Axyz
axyz0 = MkAxyz
        7
--        (S.fromList [MkXyz 1 2 3])
        (MkXyz 1 2 3 (MkXyz 4 5 6 (MkXyz 7 8 9 10)))
        (MkXyz 1 2 3 4)

xyz0 :: Xyz Double
xyz0 = MkXyz 1 2 3 0.1

incrementAxyz :: Axyz -> Axyz
incrementAxyz (MkAxyz a _ _) = MkAxyz
                             (a+0.2)
                             (xyz' (xyz' (xyz' (sin (3*a)))))
                             (xyz' (sin (2*a)))
  where
    xyz' w = MkXyz (sin a) (cos a) (sin a * cos a) w

incrementXyz :: Xyz a -> Xyz a
incrementXyz (MkXyz a _ _ b) = MkXyz (a+0.3) (2 * sin a) (3 * cos a) b

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
