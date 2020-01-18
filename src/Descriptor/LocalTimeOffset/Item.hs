module Descriptor.LocalTimeOffset.Item (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.CountryCode as CountryCode

class Class a where
  country_code               :: a -> CountryCode.Data
  country_region_id          :: a -> Word8
  reserved                   :: a -> Bool
  local_time_offset_polarity :: a -> Bool
  local_time_offset          :: a -> Word16
  time_of_change             :: a -> Word64
  next_time_offset           :: a -> Word16
  
data Data = MkData {
  _country_code               :: CountryCode.Data,
  _country_region_id          :: Word8,
  _reserved                   :: Bool,
  _local_time_offset_polarity :: Bool,
  _local_time_offset          :: Word16,
  _time_of_change             :: Word64,
  _next_time_offset           :: Word16
  }

instance Class Data where
  country_code               = _country_code
  country_region_id          = _country_region_id
  reserved                   = _reserved
  local_time_offset_polarity = _local_time_offset_polarity
  local_time_offset          = _local_time_offset
  time_of_change             = _time_of_change
  next_time_offset           = _next_time_offset

