{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import qualified Control.Concurrent as CC
import GHC.Generics ( Generic )
--import qualified System.Remote.Monitoring as EKG

import PlotHo ( Lookup, XAxisType(..), runPlotter, addHistoryChannel )

data Foo a = MkFoo { x :: Double
                   , y :: Double
                   , z :: Double
                   , w :: a
                   } deriving Generic
data Bar = MkBar { lol :: Double
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
--  (S.fromList [MkFoo 1 2 3])
  (MkFoo 1 2 3 (MkFoo 4 5 6 (MkFoo 7 8 9 10)))
  (MkFoo 1 2 3 4)

foo0 :: Foo Double
foo0 = MkFoo 1 2 3 0.1

incrementBar :: Bar -> Bar
incrementBar (MkBar a _ _) =
  MkBar
  (a+0.2)
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

  runPlotter $ do
    addHistoryChannel "Foo (XAxisTime)"   XAxisTime   $ channelWriter 0 50000 incrementFoo foo0
    addHistoryChannel "Bar (XAxisCount)"  XAxisCount  $ channelWriter 0 60000 incrementBar bar0
    addHistoryChannel "Foo (XAxisTime0)"  XAxisTime0  $ channelWriter 0 50000 incrementFoo foo0
    addHistoryChannel "Bar (XAxisCount0)" XAxisCount0 $ channelWriter 0 600000 incrementBar bar0
