module Descriptor.AVC_Timing_HRD (
  Base(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  hrd_management_valid_flag :: a -> Bool
--  reserved :: a -> Word8
  picture_and_timing_info_present :: a -> Bool
  _90kHz_flag :: a -> Maybe Bool
-- reserved :: a -> Word8
  _N :: a -> Maybe Word32
  _K :: a -> Maybe Word32
  num_units_in_tick :: a -> Word32
  fixed_farme_rate_flag :: a -> Bool
  temporal_poc_flag :: a -> Bool
  picture_to_display_conversion_flag :: a -> Bool
-- reserved :: a -> Word8

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _hrd_management_valid_flag :: Bool,
--  reserved :: a -> Word8
  _picture_and_timing_info_present :: Bool,
  __90kHz_flag :: Maybe Bool,
-- reserved :: a -> Word8
  __N :: Maybe Word32,
  __K :: Maybe Word32,
  _num_units_in_tick :: Word32,
  _fixed_farme_rate_flag :: Bool,
  _temporal_poc_flag :: Bool,
  _picture_to_display_conversion_flag :: Bool
  }

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Class Data where
  hrd_management_valid_flag          = _hrd_management_valid_flag
  picture_and_timing_info_present    = _picture_and_timing_info_present
  _90kHz_flag                        = __90kHz_flag
  _N                                 = __N
  _K                                 = __K
  num_units_in_tick                  = _num_units_in_tick
  fixed_farme_rate_flag              = _fixed_farme_rate_flag
  temporal_poc_flag                  = _temporal_poc_flag
  picture_to_display_conversion_flag = _picture_to_display_conversion_flag
