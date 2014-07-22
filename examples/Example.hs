{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import qualified Control.Concurrent as CC
import GHC.Generics ( Generic )
--import qualified System.Remote.Monitoring as EKG

import Plotter ( Lookup, SignalTree, runPlotter, addChannel, makeSignalTree )

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
channelWriter :: Int -> (a -> a) -> a -> (a -> IO ()) -> IO ()
channelWriter delay increment x' chan = do
  CC.threadDelay delay
  chan (increment x')
  channelWriter delay increment (increment x') chan

ast :: SignalTree Axyz
ast = makeSignalTree axyz0

st :: SignalTree Xyz
st = makeSignalTree xyz0

main :: IO ()
main = do
--  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000

  runPlotter $ do
    addChannel "posPlus" ast (\w _ -> channelWriter 50000 incrementAxyz axyz0 w)
    addChannel "pos" st (\w _ -> channelWriter 60000 incrementXyz xyz0 w)
