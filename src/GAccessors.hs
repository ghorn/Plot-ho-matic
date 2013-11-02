{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -ddump-deriv #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module GAccessors ( AccessorTree(..), accessors ) where

import GHC.Generics

showAccTree :: String -> AccessorTree a -> [String]
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

instance Show (AccessorTree a) where
  show = unlines . showAccTree ""

data AccessorTree a = Data String (AccessorTree a)
                    | Constructor String (AccessorTree a)
                    | Product (AccessorTree a) (AccessorTree a)
                    | Selector String (AccessorTree a)
                    | Getter (a -> Double)

accessors :: Lookup a => a -> AccessorTree a
accessors = flip toAccessorTree id

class Lookup a where
  toAccessorTree :: a -> (b -> a) -> AccessorTree b

  default toAccessorTree :: (Generic a, GLookup (Rep a)) => a -> (b -> a) -> AccessorTree b
  toAccessorTree x f = gtoAccessorTree (from x) (from . f)

class GLookup f where
  gtoAccessorTree :: f a -> (b -> f a) -> AccessorTree b

instance Lookup Float where
  toAccessorTree _ f = Getter $ realToFrac . f
instance Lookup Double where
  toAccessorTree _ f = Getter $ realToFrac . f
instance Lookup Int where
  toAccessorTree _ f = Getter $ fromIntegral . f

instance (Lookup f, Generic f) => GLookup (Rec0 f) where
  gtoAccessorTree x f = toAccessorTree (unK1 x) (unK1 . f)

instance (GLookup f, GLookup g) => GLookup (f :*: g) where
  gtoAccessorTree (x :*: y) f = Product tf tg
    where
      tf = gtoAccessorTree x $ left . f
      tg = gtoAccessorTree y $ right . f

      left  ( x' :*: _  ) = x'
      right ( _  :*: y' ) = y'

instance (Selector s, GLookup a) => GLookup (S1 s a) where
  gtoAccessorTree x f = Selector (selName x) $ gtoAccessorTree (unM1 x) (unM1 . f)

instance (Constructor c, GLookup a) => GLookup (C1 c a) where
  gtoAccessorTree x f = Constructor (conName x) $ gtoAccessorTree (unM1 x) (unM1 . f)

instance (Datatype d, GLookup (C1 c a)) => GLookup (D1 d (C1 c a)) where
  gtoAccessorTree x f = Data (datatypeName x) $ gtoAccessorTree (unM1 x) (unM1 . f)

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
