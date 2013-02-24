{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Kite.TestMessages (TestMessages(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data TestMessages = TestMessages{a_double :: !P'.Double, a_float :: !P'.Float, a_int32 :: !P'.Int32, a_int64 :: !P'.Int64,
                                 a_uint32 :: !P'.Word32, a_uint64 :: !P'.Word64, a_sint32 :: !P'.Int32, a_sint64 :: !P'.Int64,
                                 a_fixed32 :: !P'.Word32, a_fixed64 :: !P'.Word64, a_sfixed32 :: !P'.Int32, a_sfixed64 :: !P'.Int64,
                                 a_bool :: !P'.Bool, a_string :: !P'.Utf8, a_bytes :: !P'.ByteString,
                                 a_maybeDouble :: !(P'.Maybe P'.Double), a_repeatedDouble :: !(P'.Seq P'.Double)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable TestMessages where
  mergeAppend (TestMessages x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17)
   (TestMessages y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14 y'15 y'16 y'17)
   = TestMessages (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
      (P'.mergeAppend x'12 y'12)
      (P'.mergeAppend x'13 y'13)
      (P'.mergeAppend x'14 y'14)
      (P'.mergeAppend x'15 y'15)
      (P'.mergeAppend x'16 y'16)
      (P'.mergeAppend x'17 y'17)
 
instance P'.Default TestMessages where
  defaultValue
   = TestMessages P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire TestMessages where
  wireSize ft' self'@(TestMessages x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 1 x'1 + P'.wireSizeReq 1 2 x'2 + P'.wireSizeReq 1 5 x'3 + P'.wireSizeReq 1 3 x'4 +
             P'.wireSizeReq 1 13 x'5
             + P'.wireSizeReq 1 4 x'6
             + P'.wireSizeReq 1 17 x'7
             + P'.wireSizeReq 1 18 x'8
             + P'.wireSizeReq 1 7 x'9
             + P'.wireSizeReq 1 6 x'10
             + P'.wireSizeReq 1 15 x'11
             + P'.wireSizeReq 1 16 x'12
             + P'.wireSizeReq 1 8 x'13
             + P'.wireSizeReq 1 9 x'14
             + P'.wireSizeReq 1 12 x'15
             + P'.wireSizeOpt 2 1 x'16
             + P'.wireSizeRep 2 1 x'17)
  wirePut ft' self'@(TestMessages x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17)
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
             P'.wirePutReq 21 2 x'2
             P'.wirePutReq 24 5 x'3
             P'.wirePutReq 32 3 x'4
             P'.wirePutReq 40 13 x'5
             P'.wirePutReq 48 4 x'6
             P'.wirePutReq 56 17 x'7
             P'.wirePutReq 64 18 x'8
             P'.wirePutReq 77 7 x'9
             P'.wirePutReq 81 6 x'10
             P'.wirePutReq 93 15 x'11
             P'.wirePutReq 97 16 x'12
             P'.wirePutReq 104 8 x'13
             P'.wirePutReq 114 9 x'14
             P'.wirePutReq 122 12 x'15
             P'.wirePutOpt 129 1 x'16
             P'.wirePutRep 137 1 x'17
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> Prelude'.fmap (\ !new'Field -> old'Self{a_double = new'Field}) (P'.wireGet 1)
             21 -> Prelude'.fmap (\ !new'Field -> old'Self{a_float = new'Field}) (P'.wireGet 2)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{a_int32 = new'Field}) (P'.wireGet 5)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{a_int64 = new'Field}) (P'.wireGet 3)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{a_uint32 = new'Field}) (P'.wireGet 13)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{a_uint64 = new'Field}) (P'.wireGet 4)
             56 -> Prelude'.fmap (\ !new'Field -> old'Self{a_sint32 = new'Field}) (P'.wireGet 17)
             64 -> Prelude'.fmap (\ !new'Field -> old'Self{a_sint64 = new'Field}) (P'.wireGet 18)
             77 -> Prelude'.fmap (\ !new'Field -> old'Self{a_fixed32 = new'Field}) (P'.wireGet 7)
             81 -> Prelude'.fmap (\ !new'Field -> old'Self{a_fixed64 = new'Field}) (P'.wireGet 6)
             93 -> Prelude'.fmap (\ !new'Field -> old'Self{a_sfixed32 = new'Field}) (P'.wireGet 15)
             97 -> Prelude'.fmap (\ !new'Field -> old'Self{a_sfixed64 = new'Field}) (P'.wireGet 16)
             104 -> Prelude'.fmap (\ !new'Field -> old'Self{a_bool = new'Field}) (P'.wireGet 8)
             114 -> Prelude'.fmap (\ !new'Field -> old'Self{a_string = new'Field}) (P'.wireGet 9)
             122 -> Prelude'.fmap (\ !new'Field -> old'Self{a_bytes = new'Field}) (P'.wireGet 12)
             129 -> Prelude'.fmap (\ !new'Field -> old'Self{a_maybeDouble = Prelude'.Just new'Field}) (P'.wireGet 1)
             137 -> Prelude'.fmap (\ !new'Field -> old'Self{a_repeatedDouble = P'.append (a_repeatedDouble old'Self) new'Field})
                     (P'.wireGet 1)
             138 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{a_repeatedDouble = P'.mergeAppend (a_repeatedDouble old'Self) new'Field})
                     (P'.wireGetPacked 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> TestMessages) TestMessages where
  getVal m' f' = f' m'
 
instance P'.GPB TestMessages
 
instance P'.ReflectDescriptor TestMessages where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [9, 21, 24, 32, 40, 48, 56, 64, 77, 81, 93, 97, 104, 114, 122])
      (P'.fromDistinctAscList [9, 21, 24, 32, 40, 48, 56, 64, 77, 81, 93, 97, 104, 114, 122, 129, 137, 138])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".kite.TestMessages\", haskellPrefix = [], parentModule = [MName \"Kite\"], baseName = MName \"TestMessages\"}, descFilePath = [\"Kite\",\"TestMessages.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_double\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_double\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_float\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_float\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 21}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_int32\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_int32\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_int64\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_int64\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_uint32\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_uint32\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_uint64\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_uint64\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_sint32\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_sint32\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 17}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_sint64\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_sint64\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 64}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_fixed32\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_fixed32\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 77}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 7}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_fixed64\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_fixed64\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 81}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 6}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_sfixed32\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_sfixed32\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 93}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 15}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_sfixed64\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_sfixed64\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 97}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 16}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_bool\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_bool\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 104}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_string\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_string\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 114}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_bytes\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_bytes\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 122}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_maybeDouble\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_maybeDouble\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 129}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".kite.TestMessages.a_repeatedDouble\", haskellPrefix' = [], parentModule' = [MName \"Kite\",MName \"TestMessages\"], baseName' = FName \"a_repeatedDouble\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 137}, packedTag = Just (WireTag {getWireTag = 137},WireTag {getWireTag = 138}), wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"