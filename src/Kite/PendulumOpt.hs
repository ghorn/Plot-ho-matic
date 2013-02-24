{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Kite.PendulumOpt (PendulumOpt(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data PendulumOpt = PendulumOpt{x :: !(P'.Seq P'.Double), z :: !(P'.Seq P'.Double), messages :: !(P'.Seq P'.Utf8)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable PendulumOpt where
  mergeAppend (PendulumOpt x'1 x'2 x'3) (PendulumOpt y'1 y'2 y'3)
   = PendulumOpt (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default PendulumOpt where
  defaultValue = PendulumOpt P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire PendulumOpt where
  wireSize ft' self'@(PendulumOpt x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 1 x'1 + P'.wireSizeRep 1 1 x'2 + P'.wireSizeRep 1 9 x'3)
  wirePut ft' self'@(PendulumOpt x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 9 1 x'1
             P'.wirePutRep 17 1 x'2
             P'.wirePutRep 26 9 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> Prelude'.fmap (\ !new'Field -> old'Self{x = P'.append (x old'Self) new'Field}) (P'.wireGet 1)
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{x = P'.mergeAppend (x old'Self) new'Field}) (P'.wireGetPacked 1)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{z = P'.append (z old'Self) new'Field}) (P'.wireGet 1)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{z = P'.mergeAppend (z old'Self) new'Field}) (P'.wireGetPacked 1)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{messages = P'.append (messages old'Self) new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> PendulumOpt) PendulumOpt where
  getVal m' f' = f' m'
 
instance P'.GPB PendulumOpt
 
instance P'.ReflectDescriptor PendulumOpt where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [9, 10, 17, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".kite.PendulumOpt\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"PendulumOpt\"}, descFilePath = [\"Kite\",\"PendulumOpt.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.PendulumOpt.x\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"PendulumOpt\"], baseName' = FName \"x\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Just (WireTag {getWireTag = 9},WireTag {getWireTag = 10}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.PendulumOpt.z\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"PendulumOpt\"], baseName' = FName \"z\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Just (WireTag {getWireTag = 17},WireTag {getWireTag = 18}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.PendulumOpt.messages\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"PendulumOpt\"], baseName' = FName \"messages\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"