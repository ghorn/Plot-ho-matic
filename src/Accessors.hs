{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}
{-# Language ExistentialQuantification #-}

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

atToPbt :: ExpQ -> AccessorTree -> ExpQ
atToPbt getDouble (APrim pbCon) = [| PbtGetter ($pbCon . $getDouble) |]
atToPbt getStruct (AStruct forest) = [| PbtStruct (zip strNames $forestQ) |]
  where
    forestQ = listE $ zipWith (\n t -> atToPbt [| $(varE n) . $getStruct |] t) names trees
    (names,trees) = unzip forest
    strNames = map nameBase names
atToPbt getSeq   (ASeq   pbf) = [| PbtFunctor PbSeq   $getSeq   $(atToPbt [| id |] pbf) |]
atToPbt getMaybe (AMaybe pbf) = [| PbtFunctor PbMaybe $getMaybe $(atToPbt [| id |] pbf) |]


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
getPbPrim :: Name -> Q (Maybe ExpQ)
getPbPrim name = case M.lookup name pbPrimMap of
  x@(Just _) -> return x
  Nothing -> do
    isEnum <- isInstance ''Enum [ConT name]
    if isEnum
      then return $ Just [| PbEnum . (\x -> (fromEnum x, show x)) |]
      else return Nothing


-- | take a constructor field and return usable stuff
handleField :: Type -> Q AccessorTree
handleField (ConT type') = do
  let safeGetInfo :: Q [Con]
      safeGetInfo = do
        info <- reify type'
        case info of
          (TyConI (DataD _ _ _ [constructor] _ )) -> return [constructor]
          (TyConI (NewtypeD _ _ _ constructor _ )) -> return [constructor]
          (TyConI (DataD _ _ _ constructors _ )) -> return constructors
          d -> error $ "handleField: safeGetInfo got unsafe info: " ++ show d
  constructors <- safeGetInfo
  case constructors of
    -- recursive protobuf
    [RecC _ varStrictTypes] -> do
      let (names,types) = unzip $ map (\(x,_,z) -> (x,z)) varStrictTypes
      outputs  <- mapM handleField types
      return $ AStruct (zip names outputs)

    -- everything else
    xx -> do
      maybePrim <- getPbPrim type'
      let msg = "can't find appropriate PbPrim for " ++ show type' ++ "\n" ++ show xx
          con = fromMaybe (error msg) maybePrim

      return (APrim con)
-- handle optional fields
handleField x@(AppT (ConT con) (ConT type'))
  | con == ''Maybe  = fmap AMaybe $ handleField (ConT type')
  | con == ''P'.Seq = fmap ASeq   $ handleField (ConT type')
  | otherwise = error $ "handleField (AppT ...): can't handle constructor "++show con++" in "++show x
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
  atToPbt [| id |] outputs'
