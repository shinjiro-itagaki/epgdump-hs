module Descriptor.Linkage where
import Descriptor.Common(Base,TOS,HasPrivateDataBytes)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, TOS a, HasPrivateDataBytes a) => Linkage a where
--  transport_stream_id :: a -> Word16
--  original_network_id :: a -> Word16
--  service_id          :: a -> Word16
  linkage_type        :: a -> Word8
--  private_data_bytes  :: a -> [Word8]

