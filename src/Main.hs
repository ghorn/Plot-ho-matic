{-# OPTIONS_GHC -Wall #-}
{-# Language CPP #-}
{-# Language DoAndIfThenElse #-}
{-# Language TemplateHaskell #-}
--{-# OPTIONS_GHC -ddump-splices #-}
--{-# Language OverloadedStrings #-}

module Main ( main ) where

#if OSX
import qualified System.ZMQ3 as ZMQ
#else
import qualified System.ZMQ as ZMQ
#endif
import qualified Control.Concurrent as CC
import Control.Monad ( forever )
import qualified Data.ByteString.Lazy as BL
import qualified Text.ProtocolBuffers as PB

--import qualified System.Remote.Monitoring as EKG

import qualified Kite.MultiCarousel as MC

import ParseArgs ( getip )
import Plotter ( runPlotter, newChannel, makeAccessors )

main :: IO ()
main = do
--  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000
  ip <- getip "multicarousel" "tcp://localhost:5563"
  putStrLn $ "using ip \""++ip++"\""
  
  (c0, chan0) <- newChannel "carousel" $(makeAccessors ''MC.MultiCarousel)
  listenerTid <- CC.forkIO (sub ip chan0)
  
  runPlotter [c0] [ listenerTid
--                  , ekgTid
                  ]

sub :: (PB.Wire a, PB.ReflectDescriptor a) => String -> CC.Chan a -> IO ()
sub ip chan = ZMQ.withContext 1 $ \context -> do
#if OSX
  let receive = ZMQ.receive
#else
  let receive = flip ZMQ.receive []
#endif
  ZMQ.withSocket context ZMQ.Sub $ \subscriber -> do
    ZMQ.connect subscriber ip
    ZMQ.subscribe subscriber "multi-carousel"
    forever $ do
      _ <- receive subscriber
      mre <- ZMQ.moreToReceive subscriber
      if mre
      then do
        msg <- receive subscriber
        let cs = case PB.messageGet (BL.fromChunks [msg]) of
              Left err -> error err
              Right (cs',_) -> cs'
        CC.writeChan chan cs
      else return ()
