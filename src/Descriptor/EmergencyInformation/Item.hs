module Descriptor.EmergencyInformation.Item (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.AreaCode as AreaCode
  
class Class a where
  service_id          :: a -> Word16
  start_end_flag      :: a -> Bool
  signal_level        :: a -> Bool
  reserved_future_use :: a -> Word8
  area_code_length    :: a -> Word8
  area_codes          :: a -> [AreaCode.Data]

data Data = MkData {
  _service_id          :: Word16,
  _start_end_flag      :: Bool,
  _signal_level        :: Bool,
  _reserved_future_use :: Word8,
  _area_code_length    :: Word8,
  _area_codes          :: Vector AreaCode.Data
  }

instance Class Data where
  service_id          = _service_id
  start_end_flag      = _start_end_flag
  signal_level        = _signal_level
  reserved_future_use = _reserved_future_use
  area_code_length    = _area_code_length
  area_codes          = toList . _area_codes
