{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}

import Control.Concurrent ( forkIO, threadDelay )

import PlotterGL
import Quotes --( f, MyType(..) )


data Xyz = MkXyz { x_ :: Double
                 , y_ :: Double
                 , z_ :: Double
                 }
data Axyz = MkAxyz { a_ :: Double
                   , xyz_ :: Xyz
                   }

anAxyz :: Axyz
anAxyz = MkAxyz 7 (MkXyz 1 2 3)

increment :: Axyz -> Axyz
increment (MkAxyz a (MkXyz _ _ _)) = MkAxyz (a+0.1) (MkXyz (sin a) (cos a) ((sin a)*(cos a)))

updateLoop :: Int -> Axyz -> (Axyz -> IO ()) -> IO ()
updateLoop n anAxyz' receiveNewMessage = do
  receiveNewMessage anAxyz'
  putStrLn $ "update " ++ show n
  threadDelay 1000000
  updateLoop (n+1::Int) (increment anAxyz') receiveNewMessage

main :: IO ()
main = do
  (receiveNewMessage, infos) <- $(setupTelem "position" ''Axyz)

  updateLoopId <- forkIO $ updateLoop 0 anAxyz receiveNewMessage

  runPlotter infos [updateLoopId]
