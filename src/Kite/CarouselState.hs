{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Kite.CarouselState (CarouselState(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Kite.Dcm as Kite (Dcm)
import qualified Kite.KiteOutputs as Kite (KiteOutputs)
import qualified Kite.Xyz as Kite (Xyz)
 
data CarouselState = CarouselState{kiteXyz :: !Kite.Xyz, kiteDcm :: !Kite.Dcm, delta :: !P'.Double, rArm :: !P'.Double,
                                   zt :: !P'.Double, messages :: !(P'.Seq P'.Utf8), w0 :: !(P'.Maybe P'.Double),
                                   kiteTransparency :: !(P'.Maybe P'.Double), lineTransparency :: !(P'.Maybe P'.Double),
                                   visSpan :: !(P'.Maybe P'.Double), outputs :: !(P'.Maybe Kite.KiteOutputs)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable CarouselState where
  mergeAppend (CarouselState x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   (CarouselState y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11)
   = CarouselState (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
 
instance P'.Default CarouselState where
  defaultValue
   = CarouselState P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire CarouselState where
  wireSize ft' self'@(CarouselState x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 11 x'1 + P'.wireSizeReq 1 11 x'2 + P'.wireSizeReq 1 1 x'3 + P'.wireSizeReq 1 1 x'4 +
             P'.wireSizeReq 1 1 x'5
             + P'.wireSizeRep 1 9 x'6
             + P'.wireSizeOpt 1 1 x'7
             + P'.wireSizeOpt 1 1 x'8
             + P'.wireSizeOpt 1 1 x'9
             + P'.wireSizeOpt 1 1 x'10
             + P'.wireSizeOpt 1 11 x'11)
  wirePut ft' self'@(CarouselState x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 11 x'1
             P'.wirePutReq 18 11 x'2
             P'.wirePutReq 25 1 x'3
             P'.wirePutReq 33 1 x'4
             P'.wirePutReq 41 1 x'5
             P'.wirePutRep 50 9 x'6
             P'.wirePutOpt 57 1 x'7
             P'.wirePutOpt 65 1 x'8
             P'.wirePutOpt 73 1 x'9
             P'.wirePutOpt 81 1 x'10
             P'.wirePutOpt 90 11 x'11
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{kiteXyz = P'.mergeAppend (kiteXyz old'Self) (new'Field)}) (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{kiteDcm = P'.mergeAppend (kiteDcm old'Self) (new'Field)}) (P'.wireGet 11)
             25 -> Prelude'.fmap (\ !new'Field -> old'Self{delta = new'Field}) (P'.wireGet 1)
             33 -> Prelude'.fmap (\ !new'Field -> old'Self{rArm = new'Field}) (P'.wireGet 1)
             41 -> Prelude'.fmap (\ !new'Field -> old'Self{zt = new'Field}) (P'.wireGet 1)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{messages = P'.append (messages old'Self) new'Field}) (P'.wireGet 9)
             57 -> Prelude'.fmap (\ !new'Field -> old'Self{w0 = Prelude'.Just new'Field}) (P'.wireGet 1)
             65 -> Prelude'.fmap (\ !new'Field -> old'Self{kiteTransparency = Prelude'.Just new'Field}) (P'.wireGet 1)
             73 -> Prelude'.fmap (\ !new'Field -> old'Self{lineTransparency = Prelude'.Just new'Field}) (P'.wireGet 1)
             81 -> Prelude'.fmap (\ !new'Field -> old'Self{visSpan = Prelude'.Just new'Field}) (P'.wireGet 1)
             90 -> Prelude'.fmap (\ !new'Field -> old'Self{outputs = P'.mergeAppend (outputs old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> CarouselState) CarouselState where
  getVal m' f' = f' m'
 
instance P'.GPB CarouselState
 
instance P'.ReflectDescriptor CarouselState where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [10, 18, 25, 33, 41])
      (P'.fromDistinctAscList [10, 18, 25, 33, 41, 50, 57, 65, 73, 81, 90])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".kite.CarouselState\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"CarouselState\"}, descFilePath = [\"Kite\",\"CarouselState.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.kiteXyz\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"kiteXyz\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".kite.Xyz\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"Xyz\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.kiteDcm\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"kiteDcm\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".kite.Dcm\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"Dcm\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.delta\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"delta\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 25}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.rArm\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"rArm\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 33}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.zt\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"zt\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 41}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.messages\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"messages\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.w0\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"w0\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 57}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.kiteTransparency\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"kiteTransparency\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 65}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.lineTransparency\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"lineTransparency\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 73}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.visSpan\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"visSpan\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 81}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.CarouselState.outputs\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"CarouselState\"], baseName' = FName \"outputs\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".kite.KiteOutputs\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"KiteOutputs\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"