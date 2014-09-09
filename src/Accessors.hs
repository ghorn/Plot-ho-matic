{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -ddump-deriv #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE DeriveGeneric #-} -- for example at bottom

module Accessors
       ( Generic
       , Lookup(..)
       , AccessorTree(..)
       , accessors
       , flatten
       ) where

import Data.List ( intercalate )
import qualified Linear
import GHC.Word
import Data.Int
import Foreign.C.Types
import GHC.Generics

showAccTree :: String -> AccessorTree a -> [String]
showAccTree spaces (ATGetter _) = [spaces ++ "ATGetter {}"]
showAccTree spaces (Data name trees) =
  (spaces ++ "Data " ++ show name) :
  concatMap (showChild (spaces ++ "    ")) trees

showChild :: String -> (String, AccessorTree a) -> [String]
showChild spaces (name, tree) =
  (spaces ++ name) : showAccTree (spaces ++ "    ") tree

instance Show (AccessorTree a) where
  show = unlines . showAccTree ""

data AccessorTree a = Data (String,String) [(String, AccessorTree a)]
                    | ATGetter (a -> Double)

accessors :: Lookup a => a -> AccessorTree a
accessors = flip toAccessorTree id

showMsgs :: [String] -> String
showMsgs = intercalate "."

flatten :: AccessorTree a -> [(String, a -> Double)]
flatten = flatten' []

flatten' :: [String] -> AccessorTree a -> [(String, a -> Double)]
flatten' msgs (ATGetter f) = [(showMsgs (reverse msgs), f)]
flatten' msgs (Data (_,_) trees) = concatMap f trees
  where
    f (name,tree) = flatten' (name:msgs) tree

-- | Things which you can make a tree of labeled getters for.
-- You should derive this using GHC.Generics.
class Lookup a where
  toAccessorTree :: a -> (b -> a) -> AccessorTree b

  default toAccessorTree :: (Generic a, GLookup (Rep a)) => a -> (b -> a) -> AccessorTree b
  toAccessorTree x f = gtoAccessorTree (from x) (from . f)

class GLookup f where
  gtoAccessorTree :: f a -> (b -> f a) -> AccessorTree b

class GLookupS f where
  gtoAccessorTreeS :: f a -> (b -> f a) -> [(String, AccessorTree b)]

-- some instance from linear
instance Lookup a => Lookup (Linear.V0 a) where
  toAccessorTree _ _ =
    Data ("V0", "V0") []
instance Lookup a => Lookup (Linear.V1 a) where
  toAccessorTree xyz f =
    Data ("V1", "V1") [ ("x", toAccessorTree (getX xyz) (getX . f))
                      ]
    where
      getX (Linear.V1 x) = x
instance Lookup a => Lookup (Linear.V2 a) where
  toAccessorTree xyz f =
    Data ("V2", "V2") [ ("x", toAccessorTree (getX xyz) (getX . f))
                      , ("y", toAccessorTree (getY xyz) (getY . f))
                      ]
    where
      getX (Linear.V2 x _) = x
      getY (Linear.V2 _ y) = y
instance Lookup a => Lookup (Linear.V3 a) where
  toAccessorTree xyz f =
    Data ("V3", "V3") [ ("x", toAccessorTree (getX xyz) (getX . f))
                      , ("y", toAccessorTree (getY xyz) (getY . f))
                      , ("z", toAccessorTree (getZ xyz) (getZ . f))
                      ]
    where
      getX (Linear.V3 x _ _) = x
      getY (Linear.V3 _ y _) = y
      getZ (Linear.V3 _ _ z) = z
instance Lookup a => Lookup (Linear.V4 a) where
  toAccessorTree xyz f =
    Data ("V4", "V4") [ ("x", toAccessorTree (getX xyz) (getX . f))
                      , ("y", toAccessorTree (getY xyz) (getY . f))
                      , ("z", toAccessorTree (getZ xyz) (getZ . f))
                      , ("w", toAccessorTree (getW xyz) (getW . f))
                      ]
    where
      getX (Linear.V4 x _ _ _) = x
      getY (Linear.V4 _ y _ _) = y
      getZ (Linear.V4 _ _ z _) = z
      getW (Linear.V4 _ _ _ w) = w
instance Lookup a => Lookup (Linear.Quaternion a) where
  toAccessorTree xyz f =
    Data ("Quaternion", "Quaternion")
    [ ("q0", toAccessorTree (getQ0 xyz) (getQ0 . f))
    , ("q1", toAccessorTree (getQ1 xyz) (getQ1 . f))
    , ("q2", toAccessorTree (getQ2 xyz) (getQ2 . f))
    , ("q3", toAccessorTree (getQ3 xyz) (getQ3 . f))
    ]
    where
      getQ0 (Linear.Quaternion q0 _) = q0
      getQ1 (Linear.Quaternion _ (Linear.V3 x _ _)) = x
      getQ2 (Linear.Quaternion _ (Linear.V3 _ y _)) = y
      getQ3 (Linear.Quaternion _ (Linear.V3 _ _ z)) = z


instance Lookup f => GLookup (Rec0 f) where
  gtoAccessorTree x f = toAccessorTree (unK1 x) (unK1 . f)

instance (Selector s, GLookup a) => GLookupS (S1 s a) where
  gtoAccessorTreeS x f = [(selname, gtoAccessorTree (unM1 x) (unM1 . f))]
    where
      selname = case selName x of
        [] -> "()"
        y -> y

instance GLookupS U1 where
  gtoAccessorTreeS _ _ = []

instance (GLookupS f, GLookupS g) => GLookupS (f :*: g) where
  gtoAccessorTreeS (x :*: y) f = tf ++ tg
    where
      tf = gtoAccessorTreeS x $ left . f
      tg = gtoAccessorTreeS y $ right . f

      left  ( x' :*: _  ) = x'
      right ( _  :*: y' ) = y'

instance (Datatype d, Constructor c, GLookupS a) => GLookup (D1 d (C1 c a)) where
  gtoAccessorTree d@(M1 c) f = Data (datatypeName d, conName c) con
    where
      con = gtoAccessorTreeS (unM1 c) (unM1 . unM1 . f)

-- basic types
instance Lookup () where -- hack to get dummy tree
  toAccessorTree _ _ = ATGetter $ const 0
instance Lookup Int where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Float where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Double where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Bool where
  toAccessorTree _ f = ATGetter $ realToFrac . fromEnum . f

-- Word types
instance Lookup Word where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Word8 where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Word16 where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Word32 where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Word64 where
  toAccessorTree _ f = ATGetter $ realToFrac . f

-- Int types
instance Lookup Int8 where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Int16 where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Int32 where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup Int64 where
  toAccessorTree _ f = ATGetter $ realToFrac . f

-- C types
instance Lookup CChar where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CSChar where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CUChar where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CShort where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CUShort where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CInt where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CUInt where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CLong where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CULong where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CPtrdiff where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CSize where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CWchar where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CSigAtomic where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CLLong where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CULLong where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CIntPtr where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CUIntPtr where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CIntMax where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CUIntMax where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CClock where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CTime where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CUSeconds where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CSUSeconds where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CFloat where
  toAccessorTree _ f = ATGetter $ realToFrac . f
instance Lookup CDouble where
  toAccessorTree _ f = ATGetter $ realToFrac . f


--data Xyz = Xyz { xx :: Int
--               , yy :: Double
--               , zz :: Float
--               , ww :: Int
--               } deriving (Generic)
--data One = MkOne { one :: Double } deriving (Generic)
--data Foo = MkFoo { aaa :: Int
--                 , bbb :: Xyz
--                 , ccc :: One
--                 } deriving (Generic)
--instance Lookup One
--instance Lookup Xyz
--instance Lookup Foo
--
--foo :: Foo
--foo = MkFoo 2 (Xyz 6 7 8 9) (MkOne 17)
--
--go = accessors foo
