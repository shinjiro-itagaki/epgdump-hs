module Descriptor.AVC_Video where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => AVC_Video a where
  profile_idc :: a -> Word8
  constraint_set0_flag :: a -> Bool
  constraint_set1_flag :: a -> Bool  
  constraint_set2_flag :: a -> Bool
  avc_compatible_flags :: a -> Word8
  level_idc :: a -> Word8
  avc_still_present :: a -> Bool
  avc_24_hour_picture_flag :: a -> Bool
--  reserved :: a -> Word8
