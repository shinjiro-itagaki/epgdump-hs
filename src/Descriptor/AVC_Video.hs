module Descriptor.AVC_Video (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  profile_idc :: a -> Word8
  constraint_set0_flag :: a -> Bool
  constraint_set1_flag :: a -> Bool  
  constraint_set2_flag :: a -> Bool
  avc_compatible_flags :: a -> Word8
  level_idc :: a -> Word8
  avc_still_present :: a -> Bool
  avc_24_hour_picture_flag :: a -> Bool
--  reserved :: a -> Word8

data Data = MkData {
  _descriptor_tag           :: Word8,
  _descriptor_length        :: Word8,
  _profile_idc              :: Word8,
  _constraint_set0_flag     :: Bool,
  _constraint_set1_flag     :: Bool,
  _constraint_set2_flag     :: Bool,
  _avc_compatible_flags     :: Word8,
  _level_idc                :: Word8,
  _avc_still_present        :: Bool,
  _avc_24_hour_picture_flag :: Bool
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  profile_idc              = _profile_idc
  constraint_set0_flag     = _constraint_set0_flag
  constraint_set1_flag     = _constraint_set1_flag
  constraint_set2_flag     = _constraint_set2_flag
  avc_compatible_flags     = _avc_compatible_flags
  level_idc                = _level_idc
  avc_still_present        = _avc_still_present
  avc_24_hour_picture_flag = _avc_24_hour_picture_flag
