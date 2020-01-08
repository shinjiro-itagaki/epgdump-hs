module Descriptor.TS_Information (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  remote_control_key_id   :: a -> Word8
  length_of_ts_name       :: a -> Word8
  transmission_type_count :: a -> Word8
  ts_name                 :: a -> String
  transmission_types      :: a -> [TransmissionType]

data Data = MkData {
  _descriptor_tag          :: Word8,
  _descriptor_length       :: Word8,
  _remote_control_key_id   :: Word8,
  _length_of_ts_name       :: Word8,
  _transmission_type_count :: Word8,
  _ts_name                 :: String,
  _transmission_types      :: [TransmissionType]
  }

data TransmissionType = MkTransmissionType {
  transmission_type_info :: Word8,
  num_of_service :: Word8,
  services :: [Word16]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where

instance Class Data where
  remote_control_key_id    = _remote_control_key_id
  length_of_ts_name        = _length_of_ts_name
  transmission_type_count  = _transmission_type_count
  ts_name                  = _ts_name
  transmission_types       = _transmission_types
