{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -ddump-deriv #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GAccessors where

import GHC.Generics

import Data.List

--getIndices k (Getter msgs) 

--unProduct :: AccTree -> AccTree'
--unProduct (Product x y)


--data AccTree' = Constructor' String AccTree'
--              | Getter' [String]

showMsgs :: [String] -> String
showMsgs = intercalate "."

showAccTree :: String -> AccTree a -> [String]
showAccTree spaces (Getter _) = [spaces ++ "Getter"]
showAccTree spaces (Data name tree) =
  [spaces ++ "Data " ++ name] ++
  showAccTree (spaces ++ "    ") tree
showAccTree spaces (Constructor name tree) =
  [spaces ++ "Constructor " ++ name] ++
  showAccTree (spaces ++ "    ") tree
showAccTree spaces (Selector name tree) =
  [spaces ++ "Selector " ++ name] ++
  showAccTree (spaces ++ "    ") tree
showAccTree spaces (Product x y) =
  [spaces ++ "Product"] ++
  concatMap (showAccTree (spaces ++ "    ")) [x,y]

instance Show (AccTree a) where
  show = unlines . showAccTree ""

data AccTree a where
  Data :: String -> AccTree a -> AccTree a
  Constructor :: String -> AccTree a -> AccTree a
  Product :: AccTree a -> AccTree a -> AccTree a
  Selector :: String -> AccTree a -> AccTree a
  Getter :: (a -> Double) -> AccTree a

class Lookup a where
  makeTree :: a -> (b -> a) -> AccTree b

  default makeTree :: (Generic a, GLookup (Rep a)) => a -> (b -> a) -> AccTree b
  makeTree x f = gmakeTree (from x) (from . f)

class GLookup f where
  gmakeTree :: f a -> (b -> f a) -> AccTree b

instance Lookup Float where
  makeTree _ f = Getter $ realToFrac . f
instance Lookup Double where
  makeTree _ f = Getter $ realToFrac . f
instance Lookup Int where
  makeTree _ f = Getter $ fromIntegral . f

instance (Lookup f, Generic f) => GLookup (Rec0 f) where
  gmakeTree x f = makeTree (unK1 x) (unK1 . f)

instance (GLookup f, GLookup g) => GLookup (f :*: g) where
  gmakeTree (x :*: y) f = Product tf tg
    where
      tf = gmakeTree x $ left . f
      tg = gmakeTree y $ right . f

      left  ( x' :*: _  ) = x'
      right ( _  :*: y' ) = y'

instance (Selector s, GLookup a) => GLookup (S1 s a) where
  gmakeTree x f = Selector (selName x) $ gmakeTree (unM1 x) (unM1 . f)

instance (Constructor c, GLookup a) => GLookup (C1 c a) where
  gmakeTree x f = Constructor (conName x) $ gmakeTree (unM1 x) (unM1 . f)

instance (Datatype d, GLookup (C1 c a)) => GLookup (D1 d (C1 c a)) where
  gmakeTree x f = Data (datatypeName x) $ gmakeTree (unM1 x) (unM1 . f)

data Xyz = Xyz { xx :: Int
               , yy :: Double
               , zz :: Float
               , ww :: Int
               } deriving (Generic)
data One = One { one :: Double } deriving (Generic)
data Foo = Foo { aaa :: Int
               , bbb :: Xyz
               , ccc :: One
               } deriving (Generic)
instance Lookup One
instance Lookup Xyz
instance Lookup Foo
foo :: Foo
foo = Foo 2 (Xyz 6 7 8 9) (One 17)
