{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Kite.Dcm (Dcm(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Dcm = Dcm{r11 :: !P'.Double, r12 :: !P'.Double, r13 :: !P'.Double, r21 :: !P'.Double, r22 :: !P'.Double, r23 :: !P'.Double,
               r31 :: !P'.Double, r32 :: !P'.Double, r33 :: !P'.Double}
         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Dcm where
  mergeAppend (Dcm x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (Dcm y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = Dcm (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
 
instance P'.Default Dcm where
  defaultValue
   = Dcm P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire Dcm where
  wireSize ft' self'@(Dcm x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 1 x'1 + P'.wireSizeReq 1 1 x'2 + P'.wireSizeReq 1 1 x'3 + P'.wireSizeReq 1 1 x'4 +
             P'.wireSizeReq 1 1 x'5
             + P'.wireSizeReq 1 1 x'6
             + P'.wireSizeReq 1 1 x'7
             + P'.wireSizeReq 1 1 x'8
             + P'.wireSizeReq 1 1 x'9)
  wirePut ft' self'@(Dcm x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
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
             P'.wirePutReq 33 1 x'4
             P'.wirePutReq 41 1 x'5
             P'.wirePutReq 49 1 x'6
             P'.wirePutReq 57 1 x'7
             P'.wirePutReq 65 1 x'8
             P'.wirePutReq 73 1 x'9
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> Prelude'.fmap (\ !new'Field -> old'Self{r11 = new'Field}) (P'.wireGet 1)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{r12 = new'Field}) (P'.wireGet 1)
             25 -> Prelude'.fmap (\ !new'Field -> old'Self{r13 = new'Field}) (P'.wireGet 1)
             33 -> Prelude'.fmap (\ !new'Field -> old'Self{r21 = new'Field}) (P'.wireGet 1)
             41 -> Prelude'.fmap (\ !new'Field -> old'Self{r22 = new'Field}) (P'.wireGet 1)
             49 -> Prelude'.fmap (\ !new'Field -> old'Self{r23 = new'Field}) (P'.wireGet 1)
             57 -> Prelude'.fmap (\ !new'Field -> old'Self{r31 = new'Field}) (P'.wireGet 1)
             65 -> Prelude'.fmap (\ !new'Field -> old'Self{r32 = new'Field}) (P'.wireGet 1)
             73 -> Prelude'.fmap (\ !new'Field -> old'Self{r33 = new'Field}) (P'.wireGet 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Dcm) Dcm where
  getVal m' f' = f' m'
 
instance P'.GPB Dcm
 
instance P'.ReflectDescriptor Dcm where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [9, 17, 25, 33, 41, 49, 57, 65, 73])
      (P'.fromDistinctAscList [9, 17, 25, 33, 41, 49, 57, 65, 73])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".kite.Dcm\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"Dcm\"}, descFilePath = [\"Kite\",\"Dcm.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Dcm.r11\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Dcm\"], baseName' = FName \"r11\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Dcm.r12\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Dcm\"], baseName' = FName \"r12\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Dcm.r13\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Dcm\"], baseName' = FName \"r13\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 25}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Dcm.r21\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Dcm\"], baseName' = FName \"r21\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 33}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Dcm.r22\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Dcm\"], baseName' = FName \"r22\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 41}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Dcm.r23\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Dcm\"], baseName' = FName \"r23\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 49}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Dcm.r31\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Dcm\"], baseName' = FName \"r31\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 57}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Dcm.r32\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Dcm\"], baseName' = FName \"r32\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 65}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.Dcm.r33\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"Dcm\"], baseName' = FName \"r33\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 73}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"