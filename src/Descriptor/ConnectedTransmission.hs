
-- 
module Descriptor.ConnectedTransmission (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import Data.Vector(Vector,empty,toList,snoc)

class Base.Class a => Class a where
  connected_transmission_group_id         :: a -> Word16
  segment_type                            :: a -> Word8
  modulation_type_a                       :: a -> Word8
  modulation_type_b                       :: a -> Word8
  reserved_future_use                     :: a -> Word8
  additional_connected_transmission_infos :: a -> [Word8]

data Data = MkData {
  _header                                  :: Header.Data,
  _connected_transmission_group_id         :: Word16,
  _segment_type                            :: Word8,
  _modulation_type_a                       :: Word8,
  _modulation_type_b                       :: Word8,
  _additional_connected_transmission_infos :: Vector Word8
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  connected_transmission_group_id         = _connected_transmission_group_id
  segment_type                            = _segment_type
  modulation_type_a                       = _modulation_type_a
  modulation_type_b                       = _modulation_type_b
  additional_connected_transmission_infos = toList . _additional_connected_transmission_infos
