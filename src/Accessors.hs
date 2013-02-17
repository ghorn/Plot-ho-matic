{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}
{-# Language ExistentialQuantification #-}
{-# Language MultiWayIf #-}

module Accessors ( makeAccessors ) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Language.Haskell.TH
import qualified Text.ProtocolBuffers.Header as P'

import PlotTypes ( PbTree(..), PbPrim(..) )

data AccessorTree = APrim ExpQ
                  | AStruct [(Name,AccessorTree)]
                  | ASeq AccessorTree
                  | AMaybe AccessorTree

atToPbf :: ExpQ -> AccessorTree -> ExpQ
atToPbf getDouble (APrim pbCon) = [| PbfGetter ($pbCon . $getDouble) |]
atToPbf getStruct (AStruct forest) = [| PbfStruct (zip strNames $forestQ) |]
  where
    forestQ = listE $ zipWith (\n t -> atToPbf [| $(varE n) . $getStruct |] t) names trees
    (names,trees) = unzip forest
    strNames = map nameBase names
atToPbf getSeq   (ASeq   pbf) = [| PbfSeq   $getSeq   $(atToPbf [| id |] pbf) |]
atToPbf getMaybe (AMaybe pbf) = [| PbfMaybe $getMaybe $(atToPbf [| id |] pbf) |]


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
handleField :: Type -> Q AccessorTree
handleField (ConT type') = do
  let safeGetInfo :: Q [Con]
      safeGetInfo = do
        info <- reify type'
        case info of
          (TyConI (DataD _ _ _ [constructor] _ )) -> return ([constructor])
          (TyConI (NewtypeD _ _ _ constructor _ )) -> return ([constructor])
          (TyConI (DataD _ _ _ constructors _ )) -> return (constructors)
          d -> error $ "handleField: safeGetInfo got unsafe info: " ++ show d
  constructors <- safeGetInfo
  case constructors of
    -- recursive protobuf
    [RecC _ varStrictTypes] -> do
      let (names,types) = unzip $ map (\(x,_,z) -> (x,z)) varStrictTypes
      outputs  <- mapM handleField types
      return $ AStruct (zip names outputs)

    -- everything else
    _ -> do
      let con = fromMaybe (error $ "can't find appropriate PbPrim for " ++ show type')
                (M.lookup type' pbPrimMap)

      return (APrim con)
-- handle optional fields
handleField x@(AppT (ConT con) (ConT type')) = do
  if | con == ''Maybe -> fmap AMaybe $ handleField (ConT type')
     | con == ''P'.Seq -> fmap ASeq $ handleField (ConT type')
     | otherwise -> error $ "handleField (AppT ...): can't handle constructor " ++ show con ++ " in " ++ show x
handleField x = error $ "handleField _: unhandled case: " ++ show x


-- | Take a constructor with multiple fields, call handleFields on each of them,
--   assemble the result
handleConstructor :: Con -> Q AccessorTree
handleConstructor (RecC _ varStrictTypes) = do
  let (names,types) = unzip $ map (\(x,_,z) -> (x,z)) varStrictTypes
  outputs  <- mapM handleField types
  return (AStruct (zip names outputs))
handleConstructor x = fail $ "\"" ++ show x ++ "\" is not a record syntax constructor"


makeAccessors :: Name -> Q Exp
makeAccessors typ = do
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

  outputs' <- handleConstructor constructor
  atToPbf [| id |] outputs'
