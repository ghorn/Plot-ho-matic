{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

-- export Bar/Foo to silence warnings about unused records
module Main ( Bar(..), Foo(..), main ) where

import GHC.Generics ( Generic )

import Accessors ( Lookup )
import Accessors.Dynamic
import Control.Monad ( forever, void, when )
import qualified Control.Concurrent as CC

import SetHo ( runSetter )

data Bar =
  Bar
  { bar1 :: Double
  , bar2 :: Int
  , bar3 :: Bool
  } deriving (Generic, Show)
instance Lookup Bar

data Foo a =
  Foo
  { foo1 :: Double
  , foo2 :: Int
  , fooRec0 :: a
  , fooRec1 :: a
  } deriving (Generic, Show)
instance Lookup a => Lookup (Foo a)

initialFoo :: Foo Bar
initialFoo = Foo pi 42 (Bar 21 (-2) True) (Bar 21 (-2) True)

initialBar :: Foo Int
initialBar = Foo 2.2 22 (-22) 84

main :: IO ()
main = do
  upMsg <- CC.newEmptyMVar :: IO (CC.MVar (Maybe DTree))
  downMsg <- CC.newEmptyMVar :: IO (CC.MVar DTree)
  counterVar <- CC.newMVar (0::Int)

  let upstreamProcess = do
        upValue <- CC.newMVar (Left (initialFoo {foo2 = 999})) :: IO (CC.MVar (Either (Foo Bar) (Foo Int)))
        let sendState = do
              x <- CC.readMVar upValue :: IO (Either (Foo Bar) (Foo Int))
              putStrLn "                 upstream sending state"
              let d = case x of
                    Left r -> toDData r
                    Right r -> toDData r
              CC.putMVar downMsg d

        forever $ do
          putStrLn "                 upstream waiting for message"
          msg <- CC.takeMVar upMsg
          case msg of
            Nothing -> do
              putStrLn "                 upstream got refresh request"
              CC.threadDelay 500000
              CC.modifyMVar_ counterVar (return . (+1))
              count <- CC.readMVar counterVar
              putStrLn $ "                 upstream count: " ++ show count
              when (mod count 3 == 0) $ do
                putStrLn "                 upstream swapping data"
                let swapData (Left _) = Right initialBar
                    swapData (Right _) = Left initialFoo
                CC.modifyMVar_ upValue (return . swapData)
              sendState
            Just newDVal -> do
              putStrLn "                 upstream got commit request"
              CC.threadDelay 500000
              let modify (Left oldVal) = case updateLookupable oldVal newDVal of
                    Right newVal -> return (Left newVal)
                    Left err -> do
                      putStrLn $
                        "                 upstream got update error:\n" ++
                        "                 " ++ err
                      putStrLn "newDVal:"
                      print newDVal
                      return (Left oldVal)
                  modify (Right oldVal) = case updateLookupable oldVal newDVal of
                    Right newVal -> return (Right newVal)
                    Left err -> do
                      putStrLn $
                        "                 upstream got update error:\n" ++
                        "                 " ++ err
                      return (Right oldVal)

              CC.modifyMVar_ upValue modify
              sendState

  void $ CC.forkIO upstreamProcess

  let refresh = do
        putStrLn "downstream sending refresh request"
        CC.putMVar upMsg Nothing

      commit x = void $ do
        putStrLn "downstream starting to commit"
        CC.threadDelay 500000
        void $ CC.putMVar upMsg (Just x)
        putStrLn "downstream finished commit"

      pollForNewMessage = do
        mx <- CC.tryTakeMVar downMsg
        case mx of
          Nothing -> return ()
          Just _ -> putStrLn "downstream poll got msg"
        return mx

  runSetter "settings" (toDData initialFoo) pollForNewMessage refresh commit
