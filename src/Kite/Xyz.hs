{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Kite.Xyz (Xyz(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Xyz = Xyz{x :: !P'.Double, y :: !P'.Double, z :: !P'.Double}
         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Xyz where
  mergeAppend (Xyz x'1 x'2 x'3) (Xyz y'1 y'2 y'3) = Xyz (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default Xyz where
  defaultValue = Xyz P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Xyz where
  wireSize ft' self'@(Xyz x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 1 x'1 + P'.wireSizeReq 1 1 x'2 + P'.wireSizeReq 1 1 x'3)
  wirePut ft' self'@(Xyz x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 9 1 x'1
             P'.wirePutReq 17 1 x'2
             P'.wirePutReq 25 1 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> Prelude'.fmap (\ !new'Field -> old'Self{x = new'Field}) (P'.wireGet 1)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{y = new'Field}) (P'.wireGet 1)
             25 -> Prelude'.fmap (\ !new'Field -> old'Self{z = new'Field}) (P'.wireGet 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Xyz) Xyz where
  getVal m' f' = f' m'
 
instance P'.GPB Xyz
 
instance P'.ReflectDescriptor Xyz where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [9, 17, 25]) (P'.fromDistinctAscList [9, 17, 25])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".kite.Xyz\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"Xyz\"}, descFilePath = [\"Kite\",\"Xyz.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Xyz.x\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Xyz\"], baseName' = FName \"x\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Xyz.y\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Xyz\"], baseName' = FName \"y\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Xyz.z\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Xyz\"], baseName' = FName \"z\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 25}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"