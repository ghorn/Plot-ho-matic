{-# OPTIONS_GHC -Wall #-}
{-# Language MultiWayIf #-}
{-# Language TemplateHaskell #-}

module Accessors where

import Data.Maybe ( fromMaybe )
import Language.Haskell.TH

import PlotTypes

class FieldInfos a where
  getFieldInfos :: a -> [(String, a -> PbPrim)]

data Output = Output { outputString :: String
                     , outputVar :: ExpQ
                     }
instance Show Output where
  show (Output str _) = "Output " ++ show str

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
      patternName <- newName ("_" ++ nameBase name)

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

      let lookupValueName' :: String -> Q Name
          lookupValueName' str = fmap (fromMaybe (error msg')) (lookupValueName str)
            where
              msg' = "can't lookup \""++show type'++"\" with lookupValueName \""++str++"\""

          -- container variable
          (con,constructorName') =
            if | type' == doubleName     -> ([| PbDouble |],     lookupValueName' "PCDouble")
               | type' == floatName      -> ([| PbFloat |],      lookupValueName' "PCFloat")
               | type' == int32Name      -> ([| PbInt32 |],      lookupValueName' "PCInt32")
               | type' == int64Name      -> ([| PbInt64 |],      lookupValueName' "PCInt64")
               | type' == word32Name     -> ([| PbWord32 |],     lookupValueName' "PCWord32")
               | type' == word64Name     -> ([| PbWord64 |],     lookupValueName' "PCWord64")
               | type' == boolName       -> ([| PbBool |],       lookupValueName' "PCBool")
               | type' == utf8Name       -> ([| PbUtf8 |],       lookupValueName' "PCUtf8")
               | type' == byteStringName -> ([| PbByteString |], lookupValueName' "PCByteString")
               | otherwise -> error $ "handleField: unhandled type ("++show constructors++")"

          -- human readable name
          stringName = prefix ++ nameBase name
      return (varP patternName, [Output stringName [| $(con) $(varE patternName) |] ])

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


makeAccessors :: String -> Name -> Q Exp
makeAccessors prefix typ = do
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

  let mkFieldAccessor out = [| (outStr, $(lam1E pattern outVar)) |]
        where
          outStr = outputString out
          outVar = outputVar out
--  let mkGroupAccessor out = [| (outStr, $outVar) |]
--        where
--          outStr = outputString out
--          outVar = outputVar out
--      groupAccessor = lam1E pattern
  listE (map mkFieldAccessor outputs)
