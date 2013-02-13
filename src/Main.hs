{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -ddump-splices #-}
{-# Language TemplateHaskell #-}
--{-# Language OverloadedStrings #-}

module Main ( main ) where

import qualified Control.Concurrent as CC
import qualified Data.Sequence as S
import Data.Sequence ( (|>) )
--import qualified System.Remote.Monitoring as EKG

import Plotter ( runPlotter )
import Accessors ( makeAccessors )
import PlotTypes

data Xyz = MkXyz { x_ :: Double
                 , y_ :: Double
                 , z_ :: Double
                 }
data Axyz = MkAxyz { a_ :: Double
                   , xyz_ :: Xyz
                   }

class FieldNames a where
  toFN :: a -> [(String, a -> Double)]

data FieldSettings = FieldSettings { fsHistorySize :: Int
                                   , fsShowNum :: Int
                                   }

-- a random function to write a bunch of Axyz to a chan
runProducer :: CC.Chan Axyz -> Axyz -> IO ()
runProducer chan x = do
  let increment :: Axyz -> Axyz
      increment (MkAxyz a _) = MkAxyz (a+0.5) (MkXyz (sin a) (cos a) (sin a * cos a))
  CC.threadDelay 50000
  CC.writeChan chan (increment x)
  runProducer chan (increment x)

-- reads the chan and calls receiveNewMessage
serverLoop :: CC.Chan a -> (a -> IO ()) -> IO ()
serverLoop chan receiveNewMessage = do
  val <- CC.readChan chan
  receiveNewMessage val
  serverLoop chan receiveNewMessage

receiveNewMessage' :: CC.MVar Int -> CC.MVar (S.Seq a) -> a -> IO ()
receiveNewMessage' maxNum' msgList newMsg = do
  maxNum <- CC.readMVar maxNum'
  let f seq0 = return $ S.drop (S.length seq0 + 1 - maxNum) (seq0 |> newMsg)
  CC.modifyMVar_ msgList f

main :: IO ()
main = do
--  _ <- EKG.forkServer "localhost" 8000

  chan <- CC.newChan
  producerTid <- CC.forkIO $ runProducer chan $ MkAxyz 7 (MkXyz 1 2 3)-- 4)

  maxNumMV <- CC.newMVar (100 :: Int)
  seqmv <- CC.newMVar S.empty
  serverTid <- CC.forkIO $ serverLoop chan (receiveNewMessage' maxNumMV seqmv)

--  (receiveNewMessage, infos) <- $(setupTelem "position" ''Axyz)
--  serverTid <- CC.forkIO $ serverLoop chan receiveNewMessage

  let accessors = $(makeAccessors "position" ''Axyz)
  runPlotter (Channel accessors seqmv maxNumMV) [producerTid, serverTid]
