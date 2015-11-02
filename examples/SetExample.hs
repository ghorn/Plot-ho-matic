{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

-- export Bar/Foo to silence warnings about unused records
module Main ( Bar(..), Foo(..), main ) where

import GHC.Generics ( Generic )

import Data.IORef ( newIORef, readIORef, writeIORef )

import SetHo ( runSetter )
import Accessors ( Lookup )

data Bar =
  Bar
  { bar1 :: Double
  , bar2 :: Int
  , bar3 :: Bool
  } deriving (Generic, Show)
instance Lookup Bar

data Foo =
  Foo
  { foo1 :: Double
  , foo2 :: Int
  , fooBar :: Bar
  } deriving (Generic, Show)
instance Lookup Foo

initialFoo :: Foo
initialFoo = Foo pi 42 (Bar 1 2 True)

main :: IO ()
main = do
  fooRef <- newIORef initialFoo
  let refresh = readIORef fooRef
      commit x = print x >> writeIORef fooRef x
  runSetter initialFoo refresh commit
