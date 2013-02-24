{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Kite.MultiCarousel (MultiCarousel(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Kite.CarouselState as Kite (CarouselState)
 
data MultiCarousel = MultiCarousel{css :: !(P'.Seq Kite.CarouselState), messages :: !(P'.Seq P'.Utf8)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MultiCarousel where
  mergeAppend (MultiCarousel x'1 x'2) (MultiCarousel y'1 y'2) = MultiCarousel (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default MultiCarousel where
  defaultValue = MultiCarousel P'.defaultValue P'.defaultValue
 
instance P'.Wire MultiCarousel where
  wireSize ft' self'@(MultiCarousel x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeRep 1 9 x'2)
  wirePut ft' self'@(MultiCarousel x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
             P'.wirePutRep 18 9 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{css = P'.append (css old'Self) new'Field}) (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{messages = P'.append (messages old'Self) new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> MultiCarousel) MultiCarousel where
  getVal m' f' = f' m'
 
instance P'.GPB MultiCarousel
 
instance P'.ReflectDescriptor MultiCarousel where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".kite.MultiCarousel\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"MultiCarousel\"}, descFilePath = [\"Kite\",\"MultiCarousel.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.MultiCarousel.css\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"MultiCarousel\"], baseName' = FName \"css\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".kite.CarouselState\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"CarouselState\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.MultiCarousel.messages\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"MultiCarousel\"], baseName' = FName \"messages\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"