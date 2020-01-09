module Descriptor.Linkage (
  Class(..)
  ,Data
  ) where
import Common(HasOriginalNetworkID(..))
import Descriptor.Common(Base(..),TOS(..),HasPrivateDataBytes(..),HasServiceID(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, TOS a, HasPrivateDataBytes a) => Class a where
--  transport_stream_id :: a -> Word16
--  original_network_id :: a -> Word16
--  service_id          :: a -> Word16
  linkage_type        :: a -> Word8
--  private_data_bytes  :: a -> [Word8]

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _linkage_type      :: Word8,
  _transport_stream_id :: Word16,
  _original_network_id :: Word16,
  _service_id          :: Word16,
  _private_data_bytes  :: [Word8]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance TOS Data where
  transport_stream_id  = _transport_stream_id

instance HasOriginalNetworkID Data where
  original_network_id  = _original_network_id
  
instance HasServiceID Data where
  service_id = _service_id
  
instance HasPrivateDataBytes Data where
  private_data_bytes = _private_data_bytes
  
instance Class Data where  
  linkage_type = _linkage_type

