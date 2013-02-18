{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -ddump-splices #-}
{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}

module Main ( main ) where

import qualified Control.Concurrent as CC
import Data.Sequence ( (<|) )
import Data.Sequence ( (|>) )
import qualified Data.Sequence as S
import Data.Time ( UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime )
import qualified System.Remote.Monitoring as EKG

import Plotter ( runPlotter )
import Accessors ( makeAccessors )
import PlotTypes ( Channel(..), pbTreeToTree )

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

-- a random function to write a bunch of Axyz to a chan
runProducer :: Int -> (a -> a) -> CC.Chan (UTCTime, a) -> a -> IO ()
runProducer delay inc chan x = do
  CC.threadDelay delay
  time <- getCurrentTime
  CC.writeChan chan (time, inc x)
  runProducer delay inc chan (inc x)

-- reads the chan and calls receiveNewMessage
serverLoop :: CC.Chan (UTCTime, a) -> Int -> (Int -> UTCTime -> a -> IO ()) -> IO ()
serverLoop chan k receiveNewMessage' = do
  (time, val) <- CC.readChan chan
  receiveNewMessage' k time val
  serverLoop chan (k+1) receiveNewMessage'

receiveNewMessage :: UTCTime -> CC.MVar Int -> CC.MVar (S.Seq (a,Int,NominalDiffTime)) ->
                     Int -> UTCTime -> a -> IO ()
receiveNewMessage time0 maxNum' msgList k time newMsg = do
  maxNum <- CC.readMVar maxNum'
  let f seq0 = return $ S.drop (S.length seq0 + 1 - maxNum) (seq0 |> (newMsg, k, diffUTCTime time time0))
  CC.modifyMVar_ msgList f

main :: IO ()
main = do
  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000
  time0 <- getCurrentTime
  
  chan0 <- CC.newChan
  chan1 <- CC.newChan
  
  producerTid0 <- CC.forkIO $ runProducer 50000 incrementAxyz chan0 $ MkAxyz 7 (S.fromList [MkXyz 1 2 3]) (MkXyz 1 2 3)
  producerTid1 <- CC.forkIO $ runProducer 60000 incrementXyz chan1 $ MkXyz 0 2 3

  maxNumMV0 <- CC.newMVar (10000 :: Int)
  maxNumMV1 <- CC.newMVar (10000 :: Int)
  
  seqmv0 <- CC.newMVar S.empty
  seqmv1 <- CC.newMVar S.empty
  
  serverTid0 <- CC.forkIO $ serverLoop chan0 0 (receiveNewMessage time0 maxNumMV0 seqmv0)
  serverTid1 <- CC.forkIO $ serverLoop chan1 0 (receiveNewMessage time0 maxNumMV1 seqmv1)

  let accessors0 = pbTreeToTree "posPlus" $(makeAccessors ''Axyz)
      accessors1 = pbTreeToTree "pos" $(makeAccessors ''Xyz)

      c0 = Channel "positionPlus" accessors0 seqmv0 maxNumMV0
      c1 = Channel "position"     accessors1 seqmv1 maxNumMV1
      channels = [ c0
                 , c1
                 ]
  runPlotter channels [ producerTid0
                      , producerTid1
                      , serverTid0
                      , serverTid1
                      , ekgTid
                      ]
