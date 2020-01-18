module Descriptor.TS_Information (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header


class Base.Class a => Class a where
  remote_control_key_id   :: a -> Word8
  length_of_ts_name       :: a -> Word8
  transmission_type_count :: a -> Word8
  ts_name                 :: a -> String
  transmission_types      :: a -> [TransmissionType]

data Data = MkData {
  _header                  :: Header.Data, 
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

instance Base.Class Data where

instance Class Data where
  remote_control_key_id    = _remote_control_key_id
  length_of_ts_name        = _length_of_ts_name
  transmission_type_count  = _transmission_type_count
  ts_name                  = _ts_name
  transmission_types       = _transmission_types
