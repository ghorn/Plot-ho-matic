{-# OPTIONS_GHC -Wall #-}
--{-# Language MultiWayIf #-}
{-# Language TemplateHaskell #-}

module Quotes where

import Control.Concurrent ( newMVar, modifyMVar_ )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as M
import Language.Haskell.TH

import PlotTypes

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

      containerPatName <- newName ("append_" ++ nameBase name)
      let lookupValueName' :: String -> Q Name
          lookupValueName' str = fmap (fromMaybe (error msg')) (lookupValueName str)
            where
              msg' = "can't lookup \""++show type'++"\" with lookupValueName \""++str++"\""
      let -- container variable
--          (constructorExp,constructorName') =
--            if | type' == doubleName     -> ([| PCDouble |],     lookupValueName' "PCDouble")
--               | type' == floatName      -> ([| PCFloat |],      lookupValueName' "PCFloat")
--               | type' == int32Name      -> ([| PCInt32 |],      lookupValueName' "PCInt32")
--               | type' == int64Name      -> ([| PCInt64 |],      lookupValueName' "PCInt64")
--               | type' == word32Name     -> ([| PCWord32 |],     lookupValueName' "PCWord32")
--               | type' == word64Name     -> ([| PCWord64 |],     lookupValueName' "PCWord64")
--               | type' == boolName       -> ([| PCBool |],       lookupValueName' "PCBool")
--               | type' == utf8Name       -> ([| PCUtf8 |],       lookupValueName' "PCUtf8")
--               | type' == byteStringName -> ([| PCByteString |], lookupValueName' "PCByteString")
--               | otherwise -> error $ "handleField: unhandled type ("++show constructors++")"
          nameMap = M.fromList [ (doubleName     , ([| PCDouble |],     lookupValueName' "PCDouble"))
                               , (floatName      , ([| PCFloat |],      lookupValueName' "PCFloat"))
                               , (int32Name      , ([| PCInt32 |],      lookupValueName' "PCInt32"))
                               , (int64Name      , ([| PCInt64 |],      lookupValueName' "PCInt64"))
                               , (word32Name     , ([| PCWord32 |],     lookupValueName' "PCWord32"))
                               , (word64Name     , ([| PCWord64 |],     lookupValueName' "PCWord64"))
                               , (boolName       , ([| PCBool |],       lookupValueName' "PCBool"))
                               , (utf8Name       , ([| PCUtf8 |],       lookupValueName' "PCUtf8"))
                               , (byteStringName , ([| PCByteString |], lookupValueName' "PCByteString"))
                               ]
          msg' = error $ "handleField: unhandled type ("++show constructors++")"
          (constructorExp,constructorName') = fromMaybe msg' $ M.lookup type' nameMap
      constructorName <- constructorName'

      let -- update mvar when new measurement comes in
          modify = lam1E (conP constructorName [varP containerPatName]) [| return $ $(conE constructorName) $ appendContainer $(varE patternName) $(varE containerPatName) |]
          updatemvStmt = noBindS [| modifyMVar_ $(varE mn) $(modify) |]

          -- human readable name
          stringName = prefix ++ nameBase name

          -- create a new mvar
          mkmvarStmt = bindS (varP mn) [| newMVar ($(constructorExp) emptyContainer) |]

          -- container variable
          containerExp = varE mn

      return (pattern, [Output stringName mkmvarStmt updatemvStmt containerExp])

-- handle optional fields
handleField prefix x@(name, AppT (ConT con) (ConT type')) = do
  (Just maybeName) <- lookupTypeName "Maybe"
  (Just seqName) <- lookupTypeName "P'.Seq"
--  if | con == maybeName -> do reportWarning $ "ignoring optional field \"" ++ prefix ++ nameBase name ++ "\" ("++show type'++")"
--                              return (wildP, [])
--     | con == seqName -> do reportWarning $ "ignoring repeated field \"" ++ prefix ++ nameBase name ++ "\" ("++show type'++")"
--                            return (wildP, [])
--     | otherwise -> error $ "handleField: the \"impossible\" happened in AppT " ++ show x
  if con == maybeName
    then do reportWarning $ "ignoring optional field \"" ++ prefix ++ nameBase name ++ "\" ("++show type'++")"
            return (wildP, [])
    else if con == seqName
         then do reportWarning $ "ignoring repeated field \"" ++ prefix ++ nameBase name ++ "\" ("++show type'++")"
                 return (wildP, [])
         else error $ "handleField: the \"impossible\" happened in AppT " ++ show x
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
