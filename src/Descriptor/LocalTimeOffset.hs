module Descriptor.LocalTimeOffset where
import Descriptor.Common(Base,HasCountryCode(..),CountryCode)
-- import Descriptor(AreaCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class HasCountryCode a => LocalTimeOffsetItem a where
--  country_code               :: a -> CountryCode
  country_region_id          :: a -> Word8
  reserved                   :: a -> Bool
  local_time_offset_polarity :: a -> Bool
  local_time_offset          :: a -> Word16
  time_of_change             :: a -> Word64
  next_time_offset           :: a -> Word16
  
class Base a => LocalTimeOffset a where
  items :: a -> [LocalTimeOffsetItemData]

data LocalTimeOffsetItemData = MkLocalTimeOffsetItemData {
  _local_time_offest_item_data_country_code :: CountryCode,
  _country_region_id          :: Word8,
  _reserved                   :: Bool,
  _local_time_offset_polarity :: Bool,
  _local_time_offset          :: Word16,
  _time_of_change             :: Word64,
  _next_time_offset           :: Word16
  }

  
instance HasCountryCode LocalTimeOffsetItemData where
  country_code = _local_time_offest_item_data_country_code
  
instance LocalTimeOffsetItem LocalTimeOffsetItemData where
  country_region_id          = _country_region_id
  reserved                   = _reserved
  local_time_offset_polarity = _local_time_offset_polarity
  local_time_offset          = _local_time_offset
  time_of_change             = _time_of_change
  next_time_offset           = _next_time_offset

