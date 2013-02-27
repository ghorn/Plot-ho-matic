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
import qualified Kite.CarouselState as CS

import ParseArgs ( getip )
import Plotter ( runPlotter, newChannel, makeAccessors )
import PlotTypes ( Channel(..) )

main :: IO ()
main = do
--  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000
  ip <- getip "plot-ho-matic" "tcp://localhost:5563"
  putStrLn $ "using ip \""++ip++"\""
  
  (c0, chan0) <- newChannel "multi-carousel" $(makeAccessors ''MC.MultiCarousel)
  (c1, chan1) <- newChannel "carousel" $(makeAccessors ''CS.CarouselState)
  listenerTid0 <- CC.forkIO (sub ip chan0 c0)
  listenerTid1 <- CC.forkIO (sub ip chan1 c1)
  
  runPlotter [c0,c1] [ listenerTid0
                     , listenerTid1
--                  , ekgTid
                     ]

withContext :: (ZMQ.Context -> IO a) -> IO a
#if OSX
withContext = ZMQ.withContext
#else
withContext = ZMQ.withContext 1
#endif

sub :: (PB.Wire a, PB.ReflectDescriptor a) => String -> CC.Chan a -> Channel -> IO ()
sub ip chan channel = withContext $ \context -> do
#if OSX
  let receive = ZMQ.receive
#else
  let receive = flip ZMQ.receive []
#endif
  ZMQ.withSocket context ZMQ.Sub $ \subscriber -> do
    ZMQ.connect subscriber ip
    ZMQ.subscribe subscriber (chanName channel)
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
