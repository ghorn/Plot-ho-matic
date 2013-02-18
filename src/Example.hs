{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -ddump-splices #-}
{-# Language TemplateHaskell #-}
--{-# Language OverloadedStrings #-}

module Main ( main ) where

import qualified Control.Concurrent as CC
import qualified Data.Sequence as S
import Data.Sequence ( (<|) )
--import qualified System.Remote.Monitoring as EKG

import Plotter ( runPlotter, newChannel, makeAccessors )

data Xyz = MkXyz { x_ :: Double
                 , y_ :: Double
                 , z_ :: Double
                 }
data Axyz = MkAxyz { a_ :: Double
                   , xyzList_ :: S.Seq Xyz
                   , xyz_ :: Xyz
                   }

incrementAxyz :: Axyz -> Axyz
incrementAxyz (MkAxyz a xyzs _) = MkAxyz (a+0.2) (S.take 5 (xyz <| xyzs)) xyz
  where
    xyz = MkXyz (sin a) (cos a) (sin a * cos a)

incrementXyz :: Xyz -> Xyz
incrementXyz (MkXyz a _ _) = MkXyz (a+0.3) (2 * sin a) (3 * cos a)

-- a random function to write a bunch of data to a chan
channelWriter :: Int -> (a -> a) -> CC.Chan a -> a -> IO ()
channelWriter delay increment chan x = do
  CC.threadDelay delay
  CC.writeChan chan (increment x)
  channelWriter delay increment chan (increment x)

main :: IO ()
main = do
--  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000

  (c0, chan0) <- newChannel "posPlus" $(makeAccessors ''Axyz)
  (c1, chan1) <- newChannel "pos"     $(makeAccessors ''Xyz)

  producerTid0 <- CC.forkIO $ channelWriter 50000 incrementAxyz chan0 $ MkAxyz 7 (S.fromList [MkXyz 1 2 3]) (MkXyz 1 2 3)
  producerTid1 <- CC.forkIO $ channelWriter 60000 incrementXyz chan1 $ MkXyz 0 2 3

  runPlotter [c0,c1] [ producerTid0
                     , producerTid1
--                     , ekgTid
                     ]
