{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import qualified Control.Concurrent as CC
import GHC.Generics ( Generic )
--import qualified System.Remote.Monitoring as EKG

import PlotHo ( Lookup, XAxisType(..), runPlotter, newHistoryChannel )

data Foo a = MkFoo { x :: Double
                   , y :: Double
                   , z :: Double
                   , w :: a
                   } deriving Generic
data Bar = MkBar { lol :: Double
                 , a_bool :: Bool
--                   , xyzList :: S.Seq Xyz
                 , foos :: Foo (Foo (Foo Double))
                 , foo :: Foo Double
                 } deriving Generic
instance Lookup a => Lookup (Foo a)
instance Lookup Bar

bar0 :: Bar
bar0 =
  MkBar
  7
  False
--  (S.fromList [MkFoo 1 2 3])
  (MkFoo 1 2 3 (MkFoo 4 5 6 (MkFoo 7 8 9 10)))
  (MkFoo 1 2 3 4)

foo0 :: Foo Double
foo0 = MkFoo 1 2 3 0.1

incrementBar :: Bar -> Bar
incrementBar (MkBar a b _ _) =
  MkBar
  (a+0.2)
  (not b)
  (foo' (foo' (foo' (sin (3*a)))))
  (foo' (sin (2*a)))
  where
    foo' t = MkFoo (sin a) (cos a) (sin a * cos a) t

incrementFoo :: Foo a -> Foo a
incrementFoo (MkFoo a _ _ b) = MkFoo (a+0.3) (2 * sin a) (3 * cos a) b

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

  (chan0, send0) <- newHistoryChannel "Foo (XAxisTime)"   XAxisTime
  (chan1, send1) <- newHistoryChannel "Bar (XAxisCount)"  XAxisCount
  (chan2, send2) <- newHistoryChannel "Foo (XAxisTime0)"  XAxisTime0
  (chan3, send3) <- newHistoryChannel "Bar (XAxisCount0)" XAxisCount0

  _ <- CC.forkIO $ channelWriter 0 50000 incrementFoo foo0 send0
  _ <- CC.forkIO $ channelWriter 0 60000 incrementBar bar0 send1
  _ <- CC.forkIO $ channelWriter 0 50000 incrementFoo foo0 send2
  _ <- CC.forkIO $ channelWriter 0 60000 incrementBar bar0 send3

  runPlotter Nothing [chan0, chan1, chan2, chan3]
