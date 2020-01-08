module Descriptor.ConnectedTransmission where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => ConnectedTransmission a where
  connected_transmission_group_id :: a -> Word16
  segment_type :: a -> Word8
  modulation_type_a :: a -> Word8
  modulation_type_b :: a -> Word8
--  reserved_future_use :: Word8,
  additional_connected_transmission_infos :: a -> [Word8]
