{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Kite.KiteOutputs (KiteOutputs(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data KiteOutputs = KiteOutputs{cL :: !P'.Double, cD :: !P'.Double, l_over_D :: !P'.Double, alpha_deg :: !P'.Double,
                               beta_deg :: !P'.Double, airspeed :: !P'.Double, tension :: !(P'.Maybe P'.Double),
                               power :: !(P'.Maybe P'.Double), energy :: !(P'.Maybe P'.Double), line_angle_deg :: !P'.Double,
                               r :: !P'.Double, dr :: !P'.Double, ddr :: !(P'.Maybe P'.Double)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable KiteOutputs where
  mergeAppend (KiteOutputs x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13)
   (KiteOutputs y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13)
   = KiteOutputs (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
      (P'.mergeAppend x'12 y'12)
      (P'.mergeAppend x'13 y'13)
 
instance P'.Default KiteOutputs where
  defaultValue
   = KiteOutputs P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire KiteOutputs where
  wireSize ft' self'@(KiteOutputs x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 1 x'1 + P'.wireSizeReq 1 1 x'2 + P'.wireSizeReq 1 1 x'3 + P'.wireSizeReq 1 1 x'4 +
             P'.wireSizeReq 1 1 x'5
             + P'.wireSizeReq 1 1 x'6
             + P'.wireSizeOpt 1 1 x'7
             + P'.wireSizeOpt 1 1 x'8
             + P'.wireSizeOpt 1 1 x'9
             + P'.wireSizeReq 1 1 x'10
             + P'.wireSizeReq 1 1 x'11
             + P'.wireSizeReq 1 1 x'12
             + P'.wireSizeOpt 1 1 x'13)
  wirePut ft' self'@(KiteOutputs x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13)
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
             P'.wirePutOpt 57 1 x'7
             P'.wirePutOpt 65 1 x'8
             P'.wirePutOpt 73 1 x'9
             P'.wirePutReq 81 1 x'10
             P'.wirePutReq 89 1 x'11
             P'.wirePutReq 97 1 x'12
             P'.wirePutOpt 105 1 x'13
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> Prelude'.fmap (\ !new'Field -> old'Self{cL = new'Field}) (P'.wireGet 1)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{cD = new'Field}) (P'.wireGet 1)
             25 -> Prelude'.fmap (\ !new'Field -> old'Self{l_over_D = new'Field}) (P'.wireGet 1)
             33 -> Prelude'.fmap (\ !new'Field -> old'Self{alpha_deg = new'Field}) (P'.wireGet 1)
             41 -> Prelude'.fmap (\ !new'Field -> old'Self{beta_deg = new'Field}) (P'.wireGet 1)
             49 -> Prelude'.fmap (\ !new'Field -> old'Self{airspeed = new'Field}) (P'.wireGet 1)
             57 -> Prelude'.fmap (\ !new'Field -> old'Self{tension = Prelude'.Just new'Field}) (P'.wireGet 1)
             65 -> Prelude'.fmap (\ !new'Field -> old'Self{power = Prelude'.Just new'Field}) (P'.wireGet 1)
             73 -> Prelude'.fmap (\ !new'Field -> old'Self{energy = Prelude'.Just new'Field}) (P'.wireGet 1)
             81 -> Prelude'.fmap (\ !new'Field -> old'Self{line_angle_deg = new'Field}) (P'.wireGet 1)
             89 -> Prelude'.fmap (\ !new'Field -> old'Self{r = new'Field}) (P'.wireGet 1)
             97 -> Prelude'.fmap (\ !new'Field -> old'Self{dr = new'Field}) (P'.wireGet 1)
             105 -> Prelude'.fmap (\ !new'Field -> old'Self{ddr = Prelude'.Just new'Field}) (P'.wireGet 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> KiteOutputs) KiteOutputs where
  getVal m' f' = f' m'
 
instance P'.GPB KiteOutputs
 
instance P'.ReflectDescriptor KiteOutputs where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [9, 17, 25, 33, 41, 49, 81, 89, 97])
      (P'.fromDistinctAscList [9, 17, 25, 33, 41, 49, 57, 65, 73, 81, 89, 97, 105])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".kite.KiteOutputs\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"KiteOutputs\"}, descFilePath = [\"Kite\",\"KiteOutputs.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.CL\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"cL\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.CD\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"cD\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.L_over_D\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"l_over_D\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 25}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.alpha_deg\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"alpha_deg\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 33}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.beta_deg\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"beta_deg\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 41}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.airspeed\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"airspeed\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 49}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.tension\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"tension\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 57}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.power\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"power\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 65}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.energy\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"energy\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 73}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.line_angle_deg\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"line_angle_deg\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 81}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.r\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"r\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 89}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.dr\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"dr\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 97}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.KiteOutputs.ddr\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"KiteOutputs\"], baseName' = FName \"ddr\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 105}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"