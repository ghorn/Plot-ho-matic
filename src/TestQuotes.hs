{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -ddump-splices #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
{-# Language OverloadedStrings #-}

module TestQuotes where

import Quotes
import Data.Typeable
import Data.Data
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.Sequence as Seq

--import Kite.TestMessages ( TestMessages )
import qualified Kite.CarouselState as CS
--import qualified Kite.Dcm as Dcm
--import qualified Kite.Xyz as KiteXyz

data Xyz = MkXyz { x_ :: Double
                 , y_ :: Double
                 , z_ :: Float
                 }
data Axyz = MkAxyz { a_ :: Double
                   , xyz_ :: Xyz
                   }

data TestMessages = TestMessages
                    { a_double :: !P'.Double
                    , a_float :: !P'.Float
                    , a_int32 :: !P'.Int32
                    , a_int64 :: !P'.Int64
                    , a_uint32 :: !P'.Word32
                    , a_uint64 :: !P'.Word64
                    , a_sint32 :: !P'.Int32
                    , a_sint64 :: !P'.Int64
                    , a_fixed32 :: !P'.Word32
                    , a_fixed64 :: !P'.Word64
                    , a_sfixed32 :: !P'.Int32
                    , a_sfixed64 :: !P'.Int64
                    , a_bool :: !P'.Bool
                    , a_string :: !P'.Utf8
                    , a_bytes :: !P'.ByteString
                    , a_maybeDouble :: !(P'.Maybe P'.Double)
                    , a_repeatedDouble :: !(P'.Seq P'.Double)
                    } deriving ( Show, Eq, Ord, Typeable, Data )

anTestMessages :: TestMessages
anTestMessages = TestMessages
                    { a_double = 1
                    , a_float = 2
                    , a_int32 = 3
                    , a_int64 = 4
                    , a_uint32 = 5
                    , a_uint64 = 6
                    , a_sint32 = 7
                    , a_sint64 = 8
                    , a_fixed32 = 9
                    , a_fixed64 = 10
                    , a_sfixed32 = 11
                    , a_sfixed64 = 12
                    , a_bool = True
                    , a_string = P'.uFromString "hi"
                    , a_bytes = "234554"
                    , a_maybeDouble = Just 3
--                    , a_maybeDouble = Nothing
                    , a_repeatedDouble = Seq.fromList [1,2,3]
                    }

anAxyz :: Axyz
anAxyz = MkAxyz 7 (MkXyz 1 2 3)

increment :: Axyz -> Axyz
increment (MkAxyz a (MkXyz x y z)) = MkAxyz (a-1) (MkXyz (x + 1) (y + 2) (z + 3))



go :: IO ()
go = do
  (receiveNewMessage, infos) <- $(setupTelem "position" ''Axyz)
  putStrLn "yay"

  let printLog = mapM_ printVarInfo infos

      updateLoop 0 _ = return ()
      updateLoop n anAxyz' = do
        receiveNewMessage anAxyz'
        putStrLn ""
        printLog
        updateLoop (n-1::Int) (increment anAxyz')

  printLog
  updateLoop 4 anAxyz
  --woo <- $(f ''KiteXyz.Xyz)
  --woo <- $(f ''Dcm.Dcm)
  --woo <- $(f ''CS.CarouselState)

cs :: IO ()
cs = do
  (receiveNewMessage, infos) <- $(setupTelem "carousel" ''CS.CarouselState)
  return ()

wo :: IO ()
wo = do
  (receiveNewMessage, infos) <- $(setupTelem "test" ''TestMessages)
  mapM_ printVarInfo infos
  
  receiveNewMessage anTestMessages
  putStrLn ""
  mapM_ printVarInfo infos
  
  receiveNewMessage anTestMessages
  putStrLn ""
  mapM_ printVarInfo infos
