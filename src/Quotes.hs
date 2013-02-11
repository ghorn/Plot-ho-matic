{-# OPTIONS_GHC -Wall #-}
{-# Language MultiWayIf #-}
{-# Language TemplateHaskell #-}

module Quotes where

import Control.Concurrent ( MVar, newMVar, modifyMVar_, readMVar )
import Data.Maybe ( fromMaybe )
import Data.Sequence ( Seq, (|>) )
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy as BSL
import Language.Haskell.TH
import qualified Text.ProtocolBuffers.Header as P'

-- keep this abstract so that we can use a Seq or Vector later
type ContainerType a = Seq a
emptyContainer :: ContainerType a
emptyContainer = S.empty

appendContainer :: a -> ContainerType a -> ContainerType a
appendContainer x xs = xs |> x

data PContainer = PCDouble (MVar (ContainerType Double))
                | PCFloat (MVar (ContainerType Float))
                | PCInt32 (MVar (ContainerType P'.Int32))
                | PCInt64 (MVar (ContainerType P'.Int64))
                | PCWord32 (MVar (ContainerType P'.Word32))
                | PCWord64 (MVar (ContainerType P'.Word64))
                | PCBool (MVar (ContainerType Bool))
                | PCUtf8 (MVar (ContainerType P'.Utf8))
--                | PCByteString (MVar (ContainerType (P'.ByteString)))
                | PCByteString (MVar (ContainerType (BSL.ByteString)))

data VarInfo = VarInfo String PContainer
printVarInfo :: VarInfo -> IO ()
printVarInfo (VarInfo name (PCDouble mv)) = do
  vals <- readMVar mv
  putStrLn $ "(Double)     "++ name ++ ": " ++ show vals
printVarInfo (VarInfo name (PCFloat mv)) = do
  vals <- readMVar mv
  putStrLn $ "(Float)      "++ name ++ ": " ++ show vals
printVarInfo (VarInfo name (PCInt32 mv)) = do
  vals <- readMVar mv
  putStrLn $ "(Int32)      "++ name ++ ": " ++ show vals
printVarInfo (VarInfo name (PCInt64 mv)) = do
  vals <- readMVar mv
  putStrLn $ "(Int64)      "++ name ++ ": " ++ show vals
printVarInfo (VarInfo name (PCWord32 mv)) = do
  vals <- readMVar mv
  putStrLn $ "(Word32)     "++ name ++ ": " ++ show vals
printVarInfo (VarInfo name (PCWord64 mv)) = do
  vals <- readMVar mv
  putStrLn $ "(Word64)     "++ name ++ ": " ++ show vals
printVarInfo (VarInfo name (PCBool mv)) = do
  vals <- readMVar mv
  putStrLn $ "(Bool)       "++ name ++ ": " ++ show vals
printVarInfo (VarInfo name (PCUtf8 mv)) = do
  vals <- readMVar mv
  putStrLn $ "(Utf8)       "++ name ++ ": " ++ show vals
printVarInfo (VarInfo name (PCByteString mv)) = do
  vals <- readMVar mv
  putStrLn $ "(ByteString) "++ name ++ ": " ++ show vals

data Output = Output { outputString :: String
                     , outputNewMvStmt :: StmtQ
                     , outputUpdateStmt :: StmtQ
                     , outputContainerExp :: ExpQ
                     }
instance Show Output where
  show (Output str _ _ _) = "Output " ++ show str

-- | take a constructor field and return usable stuff
handleField :: String -> (Name, Type) -> Q (PatQ, [Output])
handleField prefix (name, ConT type') = do
  let safeGetInfo :: Q (Name, [Con])
      safeGetInfo = do
      info <- reify type'
      case info of
        (TyConI (DataD _ dataName _ [constructor] _ )) -> return (dataName, [constructor])
        (TyConI (NewtypeD _ dataName _ constructor _ )) -> return (dataName, [constructor])
        (TyConI (DataD _ dataName _ constructors _ )) -> return (dataName, constructors)
        d -> error $ "handleField: safeGetInfo got unsafe info: " ++ show d
  (dataName,constructors) <- safeGetInfo
  let msg = init $ unlines
            [ "---------------- handleField: -----------------"
            , "    name: " ++ show name
            , "    dataName: " ++ show dataName
            , "    constructors: " ++ show constructors
            ]
  case constructors of
    -- recursive protobuf
    [c@(RecC {})] -> handleConstructor (prefix ++ nameBase name ++ ".") c
    -- everything else
    _ -> do
      -- make the mvar name and pattern name
      mn <- newName ("m_" ++ nameBase name)
      patternName <- newName (nameBase name)
      let pattern = varP patternName

      -- lookup some type names
--      reportWarning $ "============= dataName: " ++ show dataName

      let lookupTypeName' :: String -> Q Name
          lookupTypeName' str = fmap (fromMaybe (error msg')) (lookupTypeName str)
            where
              msg' = "can't lookup \""++show type'++"\" with lookupTypeName \""++str++"\""
      doubleName <- lookupTypeName' "Double"
      floatName <- lookupTypeName' "Float"
      int32Name <- lookupTypeName' "P'.Int32"
      int64Name <- lookupTypeName' "P'.Int64"
      word32Name <- lookupTypeName' "P'.Word32"
      word64Name <- lookupTypeName' "P'.Word64"
      boolName <- lookupTypeName' "Bool"
      utf8Name <- lookupTypeName' "P'.Utf8"
      byteStringName <- lookupTypeName' "P'.ByteString"

      let -- create a new mvar
          mkmvarStmt = bindS (varP mn) [| newMVar emptyContainer |]

          -- update mvar when new measurement comes in
          updatemvStmt =
            noBindS [| modifyMVar_ $(varE mn) (return . appendContainer $(varE patternName)) |]

          -- human readable name
          stringName = prefix ++ nameBase name
      
          -- container variable
          containerExp = if | type' == doubleName -> [| PCDouble $(varE mn) |]
                            | type' == floatName -> [| PCFloat $(varE mn) |]
                            | type' == int32Name -> [| PCInt32 $(varE mn) |]
                            | type' == int64Name -> [| PCInt64 $(varE mn) |]
                            | type' == word32Name -> [| PCWord32 $(varE mn) |]
                            | type' == word64Name -> [| PCWord64 $(varE mn) |]
                            | type' == boolName -> [| PCBool $(varE mn) |]
                            | type' == utf8Name -> [| PCUtf8 $(varE mn) |]
                            | type' == byteStringName -> [| PCByteString $(varE mn) |]
                            | otherwise -> error $ "handleField: unhandled type ("++show constructors++")"++"\n    "++msg
      return (pattern, [Output stringName mkmvarStmt updatemvStmt containerExp])
-- handle optional fields
handleField prefix x@(name, AppT (ConT con) (ConT type')) = do
  (Just maybeName) <- lookupTypeName "Maybe"
  (Just seqName) <- lookupTypeName "P'.Seq"
  if | con == maybeName -> do reportWarning $ "ignoring optional field \"" ++ prefix ++ nameBase name ++ "\" ("++show type'++")"
                              return (wildP, [])
     | con == seqName -> do reportWarning $ "ignoring repeated field \"" ++ prefix ++ nameBase name ++ "\" ("++show type'++")"
                            return (wildP, [])
     | otherwise -> error $ "handleField: the \"impossible\" happened in AppT " ++ show x
handleField _ x = error $ "handleField: the \"impossible\" happened in AppT " ++ show x


-- | Take a constructor with multiple fields, call handleFields on each of them,
--   assemble the result
handleConstructor :: String -> Con -> Q (PatQ, [Output])
handleConstructor prefix (RecC conName varStrictTypes) = do
  let varTypes = map (\(x,_,z) -> (x,z)) varStrictTypes
  (fieldPatterns,outputs)  <- fmap unzip $ mapM (handleField prefix) varTypes
  let cOutputs = concat outputs
  let conPattern = conP conName fieldPatterns
--  let msg = init $ unlines
--            [ "---------------- handleConstructor: ----------------------"
--            , "    conName: " ++ show conName ++ " ("++nameBase conName++")"
--            , "    varTypes: " ++ show varTypes
--            , "    conPattern: " ++ show conPattern
--            , "    outputs: " ++ show outputs
--            , "    cOutputs: " ++ show cOutputs
--            , "    ----------------------------------------------"
--            ]
--  reportWarning msg
  return (conPattern, cOutputs)
handleConstructor _ x = fail $ "\"" ++ show x ++ "\" is not a record syntax constructor"

setupTelem :: String -> Name -> Q Exp
setupTelem prefix typ = do
  -- get the type info
  let safeGetInfo :: Q Info
      safeGetInfo = do
      info <- reify typ
      case info of
        d@(TyConI (DataD _ _ _ [_] _ )) -> return d
        (TyConI (DataD _ typeName _ constructors _ )) ->
           error $ "setupTelem: too many constructors: " ++ show (typeName, constructors)
        d -> error $ "setupTelem: safeGetInfo got unsafe info: " ++ show d
  TyConI (DataD _ _typeName _ [constructor] _ ) <- safeGetInfo

  -- get the pattern and the names in a nice list of outputs
  (pattern, outputs) <- handleConstructor (prefix ++ ".") constructor

  -- split the outputs
  let outStrings = map outputString outputs
  let makeMVars = map outputNewMvStmt outputs
  let updateMVars = map outputUpdateStmt outputs
  let containers = map outputContainerExp outputs

  -- define the function to take new data and update the MVars
  updateFunName <- newName ("update_" ++ nameBase _typeName)
  let defUpdate = letS [funD updateFunName [clause [pattern] (normalB (doE updateMVars)) []]]

  -- define the return (...)
  let retStuff = [| return ( $(varE updateFunName)
                           , zipWith VarInfo outStrings $(listE containers)
                           ) |]
  doE $ makeMVars ++ [defUpdate, noBindS retStuff]
