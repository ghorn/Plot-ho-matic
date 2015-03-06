{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic )

import Data.Monoid ( mempty )
import Text.Printf ( printf )
import Test.Framework ( ColorMode(..), RunnerOptions'(..), TestOptions'(..), defaultMainWithOpts )
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

import Accessors

main :: IO ()
main = do
  defaultMainWithOpts
    [ accessorTests
    ]
    opts

opts :: RunnerOptions' Maybe
opts = mempty { ropt_color_mode = Just ColorAlways
              , ropt_threads = Just 1
              , ropt_test_options = Just my_test_opts
              }

my_test_opts :: TestOptions' Maybe
my_test_opts = mempty { topt_timeout = Just (Just 2000000) }


data Xyz a = Xyz { xx :: Int
                 , yy :: Double
                 , zz :: Float
                 , ww :: a
                 } deriving (Generic)
data One = MkOne { one :: Double } deriving (Generic)
data Foo = MkFoo { aaa :: Int
                 , bbb :: Xyz Int
                 , yoyo :: Xyz (Xyz Double)
                 , ccc :: One
                 } deriving (Generic)
instance Lookup One
instance Lookup a => Lookup (Xyz a)
instance Lookup Foo

yo :: Xyz (Xyz Double)
yo = Xyz 42 45 (-2000) (Xyz 2 3 4 5)

foo :: Foo
foo = MkFoo 2 (Xyz 6 7 8 9) yo (MkOne 17)

yup :: AccessorTree Foo
yup = accessors foo


accessorTests :: Test
accessorTests =
  testGroup "accessors"
  [ testCase "showTree" treeTest
  , testCase "showFlat" flatTest
  , testCase "showFlat (aligned)" flatTestAligned
  ]

assertEqualString :: String -> String -> HUnit.Assertion
assertEqualString x y = HUnit.assertBool msg (x == y)
  where
    msg = "------------------ expected: ------------------\n" ++
          x ++
          "\n------------------ but got: -------------------\n" ++
          y ++
          "\n-----------------------------------------------"

treeTest :: HUnit.Assertion
treeTest = assertEqualString x y
  where
    x = init $ unlines
        [ "MkFoo"
        , "{ aaa =  2.00e0"
        , ", bbb = Xyz"
        , "        { xx =  6.00e0"
        , "        , yy =  7.00e0"
        , "        , zz =  8.00e0"
        , "        , ww =  9.00e0"
        , "        }"
        , ", yoyo = Xyz"
        , "         { xx =  4.20e1"
        , "         , yy =  4.50e1"
        , "         , zz = -2.00e3"
        , "         , ww = Xyz"
        , "                { xx =  2.00e0"
        , "                , yy =  3.00e0"
        , "                , zz =  4.00e0"
        , "                , ww =  5.00e0"
        , "                }"
        , "         }"
        , ", ccc = MkOne"
        , "        { one =  1.70e1"
        , "        }"
        , "}"
        ]
    y = showTree yup (printf "% .2e") foo

flatTest :: HUnit.Assertion
flatTest = assertEqualString x y
  where
    x = init $ unlines
        [ "aaa =  2.00e0"
        , "bbb.xx =  6.00e0"
        , "bbb.yy =  7.00e0"
        , "bbb.zz =  8.00e0"
        , "bbb.ww =  9.00e0"
        , "yoyo.xx =  4.20e1"
        , "yoyo.yy =  4.50e1"
        , "yoyo.zz = -2.00e3"
        , "yoyo.ww.xx =  2.00e0"
        , "yoyo.ww.yy =  3.00e0"
        , "yoyo.ww.zz =  4.00e0"
        , "yoyo.ww.ww =  5.00e0"
        , "ccc.one =  1.70e1"
        ]
    y = showFlat yup False (printf "% .2e") foo

flatTestAligned :: HUnit.Assertion
flatTestAligned = assertEqualString x y
  where
    x = init $ unlines
        [ "aaa        =  2.00e0"
        , "bbb.xx     =  6.00e0"
        , "bbb.yy     =  7.00e0"
        , "bbb.zz     =  8.00e0"
        , "bbb.ww     =  9.00e0"
        , "yoyo.xx    =  4.20e1"
        , "yoyo.yy    =  4.50e1"
        , "yoyo.zz    = -2.00e3"
        , "yoyo.ww.xx =  2.00e0"
        , "yoyo.ww.yy =  3.00e0"
        , "yoyo.ww.zz =  4.00e0"
        , "yoyo.ww.ww =  5.00e0"
        , "ccc.one    =  1.70e1"
        ]
    y = showFlat yup True (printf "% .2e") foo
