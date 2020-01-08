module Descriptor.TS_Information where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => TS_Information a where
  remote_control_key_id :: a -> Word8
  length_of_ts_name :: a -> Word8
  transmission_type_count :: a -> Word8
  ts_name :: a -> String
  transmission_types :: a -> [TransmissionType]


data TransmissionType = MkTransmissionType {
  transmission_type_info :: Word8,
  num_of_service :: Word8,
  services :: [Word16]
  }
