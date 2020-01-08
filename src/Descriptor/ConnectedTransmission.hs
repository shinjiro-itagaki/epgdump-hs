module Descriptor.ConnectedTransmission (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  connected_transmission_group_id         :: a -> Word16
  segment_type                            :: a -> Word8
  modulation_type_a                       :: a -> Word8
  modulation_type_b                       :: a -> Word8
--  reserved_future_use                   :: Word8,
  additional_connected_transmission_infos :: a -> [Word8]

data Data = MkData {
  _descriptor_tag                          :: Word8,
  _descriptor_length                       :: Word8,
  _connected_transmission_group_id         :: Word16,
  _segment_type                            :: Word8,
  _modulation_type_a                       :: Word8,
  _modulation_type_b                       :: Word8,
  _additional_connected_transmission_infos :: [Word8]
  }


instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  connected_transmission_group_id         = _connected_transmission_group_id
  segment_type                            = _segment_type
  modulation_type_a                       = _modulation_type_a
  modulation_type_b                       = _modulation_type_b
  additional_connected_transmission_infos = _additional_connected_transmission_infos
