{-# OPTIONS_GHC -Wall #-}
--{-# Language MultiWayIf #-}
{-# Language TemplateHaskell #-}

module Accessors ( makeAccessors ) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Language.Haskell.TH
import qualified Text.ProtocolBuffers.Header as P'

import PlotTypes

import Data.Tree

data AccessorTree = ANode (String, ExpQ) [AccessorTree]
                  | ALeaf String ExpQ

pbPrimMap :: Map Name ExpQ
pbPrimMap =
  M.fromList [ (''Double       , [| PbDouble |])
             , (''Float        , [| PbFloat |])
             , (''P'.Int32     , [| PbInt32 |])
             , (''P'.Int64     , [| PbInt64 |])
             , (''P'.Word32    , [| PbWord32 |])
             , (''P'.Word64    , [| PbWord64 |])
             , (''Bool         , [| PbBool |])
             , (''P'.Utf8      , [| PbUtf8 |])
             , (''P'.ByteString, [| PbByteString |])
             ]

-- | take a constructor field and return usable stuff
handleField :: (Name, Type) -> Q AccessorTree
handleField (name, ConT type') = do
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
    [c@(RecC {})] -> handleConstructor (varE name) (nameBase name) c
    -- everything else
    _ -> do
      let con = fromMaybe (error $ "can't find appropriate PbPrim for " ++ show type')
                (M.lookup type' pbPrimMap)

      return $ ALeaf (nameBase name) [| $(con) . $(varE name) |]
---- handle optional fields
--handleField prefix x@(name, AppT (ConT con) (ConT type')) = do
--  (Just maybeName) <- lookupTypeName "Maybe"
--  (Just seqName) <- lookupTypeName "P'.Seq"
--  if | con == maybeName -> do reportWarning $ "ignoring optional field \"" ++ prefix ++ nameBase name ++ "\" ("++show type'++")"
--                              return (wildP, Node Nothing [])
--     | con == seqName -> do reportWarning $ "ignoring repeated field \"" ++ prefix ++ nameBase name ++ "\" ("++show type'++")"
--                            return (wildP, Node Nothing [])
--     | otherwise -> error $ "handleField: the \"impossible\" happened in AppT " ++ show x
--handleField _ x = error $ "handleField: the \"impossible\" happened in AppT " ++ show x


-- | Take a constructor with multiple fields, call handleFields on each of them,
--   assemble the result
handleConstructor :: ExpQ -> String -> Con -> Q AccessorTree
handleConstructor getter prefix (RecC _ varStrictTypes) = do
  let varTypes = map (\(x,_,z) -> (x,z)) varStrictTypes
  outputs  <- mapM handleField varTypes
  return $ ANode (prefix, getter) outputs
handleConstructor _ _ x = fail $ "\"" ++ show x ++ "\" is not a record syntax constructor"


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
  outputs' <- handleConstructor [| id |] prefix constructor
  let cat "" str = str
      cat x y = x ++ "." ++ y

      f :: ExpQ -> String -> AccessorTree -> ExpQ
      f getHigher prefix' (ANode (name,get) forest) = [| Node (name::String,Nothing) $forestQ |]
        where
          forestQ = listE $ map (f [| $get . $getHigher |] (cat prefix' name)) forest
      f getHigher prefix' (ALeaf name get) = [| Node (fullname::String, Just ($get . $getHigher)) [] |]
        where
          fullname = cat prefix' name
  f [| id |] "" outputs'
