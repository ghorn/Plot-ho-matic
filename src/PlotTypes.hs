{-# OPTIONS_GHC -Wall #-}

module PlotTypes where

import Control.Concurrent ( MVar, modifyMVar_, readMVar, swapMVar )
import Data.Sequence ( Seq, (|>) )
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy as BSL
import qualified Text.ProtocolBuffers.Header as P'

-- what the graph should draw
data GraphInfo = GraphInfo [(String, MVar PContainer)]

-- keep this abstract so that we can use a Seq or Vector later
type ContainerType a = Seq a
emptyContainer :: ContainerType a
emptyContainer = S.empty

appendContainer :: a -> ContainerType a -> ContainerType a
appendContainer x xs = xs |> x

data PContainer = PCDouble (ContainerType Double)
                | PCFloat (ContainerType Float)
                | PCInt32 (ContainerType P'.Int32)
                | PCInt64 (ContainerType P'.Int64)
                | PCWord32 (ContainerType P'.Word32)
                | PCWord64 (ContainerType P'.Word64)
                | PCBool (ContainerType Bool)
                | PCUtf8 (ContainerType P'.Utf8)
--                | PCByteString (ContainerType (P'.ByteString))
                | PCByteString (ContainerType (BSL.ByteString))

toFrac :: Fractional a => PContainer -> ContainerType a
toFrac (PCDouble c) = fmap realToFrac c
toFrac (PCFloat c) = fmap realToFrac c
toFrac (PCInt32 c) = fmap realToFrac c
toFrac (PCInt64 c) = fmap realToFrac c
toFrac (PCWord32 c) = fmap realToFrac c
toFrac (PCWord64 c) = fmap realToFrac c
toFrac (PCBool c) = fmap (\x -> if x then 1 else 0) c
--toFloat (PCUtf8 c) = fmap realToFrac c
--toFloat (PCByteString c)


clrMVar :: MVar (ContainerType a) -> IO ()
clrMVar m = swapMVar m emptyContainer >> return ()

clearPContainer :: PContainer -> PContainer
clearPContainer (PCDouble _)     = PCDouble     emptyContainer
clearPContainer (PCFloat _)      = PCFloat      emptyContainer
clearPContainer (PCInt32 _)      = PCInt32      emptyContainer
clearPContainer (PCInt64 _)      = PCInt64      emptyContainer
clearPContainer (PCWord32 _)     = PCWord32     emptyContainer
clearPContainer (PCWord64 _)     = PCWord64     emptyContainer
clearPContainer (PCBool _)       = PCBool       emptyContainer
clearPContainer (PCUtf8 _)       = PCUtf8       emptyContainer
clearPContainer (PCByteString _) = PCByteString emptyContainer

data VarInfo = VarInfo String (MVar PContainer)

clearVarInfo :: VarInfo -> IO ()
clearVarInfo (VarInfo _ mv) =  modifyMVar_ mv (return . clearPContainer)

printVarInfo :: VarInfo -> IO ()
printVarInfo (VarInfo name mv) = do
  pc <- readMVar mv
  printVarInfo' name pc

printVarInfo' :: [Char] -> PContainer -> IO ()
printVarInfo' name (PCDouble vals) = do
  putStrLn $ "(Double)     "++ name ++ ": " ++ show vals
printVarInfo' name (PCFloat vals) = do
  putStrLn $ "(Float)      "++ name ++ ": " ++ show vals
printVarInfo' name (PCInt32 vals) = do
  putStrLn $ "(Int32)      "++ name ++ ": " ++ show vals
printVarInfo' name (PCInt64 vals) = do
  putStrLn $ "(Int64)      "++ name ++ ": " ++ show vals
printVarInfo' name (PCWord32 vals) = do
  putStrLn $ "(Word32)     "++ name ++ ": " ++ show vals
printVarInfo' name (PCWord64 vals) = do
  putStrLn $ "(Word64)     "++ name ++ ": " ++ show vals
printVarInfo' name (PCBool vals) = do
  putStrLn $ "(Bool)       "++ name ++ ": " ++ show vals
printVarInfo' name (PCUtf8 vals) = do
  putStrLn $ "(Utf8)       "++ name ++ ": " ++ show vals
printVarInfo' name (PCByteString vals) = do
  putStrLn $ "(ByteString) "++ name ++ ": " ++ show vals

