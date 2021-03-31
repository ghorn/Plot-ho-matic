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
  upMsg <- CC.newEmptyMVar :: IO (CC.MVar (Maybe (Int, DTree)))
  downMsg <- CC.newEmptyMVar :: IO (CC.MVar (Int, DTree))
  counterVar <- CC.newMVar (0::Int)

  let upstreamProcess = do
        upValue <- CC.newMVar (0, Left (initialFoo {foo2 = 999})) :: IO (CC.MVar (Int, Either (Foo Bar) (Foo Int)))
        let sendState = do
              (k, x) <- CC.readMVar upValue :: IO (Int, Either (Foo Bar) (Foo Int))
              putStrLn "                 upstream sending state"
              let d = case x of
                    Left r -> toDData r
                    Right r -> toDData r
              CC.putMVar downMsg (k, d)

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
                let swapData (k, Left _) = (k, Right initialBar)
                    swapData (k, Right _) = (k, Left initialFoo)
                CC.modifyMVar_ upValue (return . swapData)
              sendState
            Just (newK, newDVal) -> do
              putStrLn "                 upstream got commit request"
              CC.threadDelay 500000
              let modify (oldK, Left oldVal) = case updateLookupable oldVal newDVal of
                    Right newVal -> return (newK, Left newVal)
                    Left err -> do
                      putStrLn $
                        "                 upstream got update error:\n" ++
                        "                 " ++ err
                      putStrLn "newDVal:"
                      print newDVal
                      return (oldK, Left oldVal)
                  modify (oldK, Right oldVal) = case updateLookupable oldVal newDVal of
                    Right newVal -> return (newK, Right newVal)
                    Left err -> do
                      putStrLn $
                        "                 upstream got update error:\n" ++
                        "                 " ++ err
                      return (oldK, Right oldVal)

              CC.modifyMVar_ upValue modify
              sendState

  void $ CC.forkIO upstreamProcess

  let refresh :: Int -> IO ()
      refresh _ = do
        putStrLn "downstream sending refresh request"
        CC.putMVar upMsg Nothing

      commit :: Int -> DTree -> IO ()
      commit k x = void $ do
        putStrLn "downstream starting to commit"
        CC.threadDelay 500000
        void $ CC.putMVar upMsg (Just (k, x))
        putStrLn "downstream finished commit"

      revertToDefaults :: Int -> DTree -> IO ()
      revertToDefaults k _x = void $ do
        putStrLn "downstream starting to revert to defaults"
        CC.threadDelay 500000
        void $ CC.putMVar upMsg (Just (k, toDData initialFoo))
        putStrLn "downstream finished revert to defaults"

      pollForNewMessage :: IO (Maybe (Int, DTree))
      pollForNewMessage = do
        mx <- CC.tryTakeMVar downMsg
        case mx of
          Nothing -> return ()
          Just _ -> putStrLn "downstream poll got msg"
        return mx

  runSetter Nothing "settings" (toDData initialFoo) pollForNewMessage refresh commit revertToDefaults
