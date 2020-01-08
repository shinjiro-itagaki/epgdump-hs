module Descriptor.AVC_Timing_HRD where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => AVC_Timing_HRD a where
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

