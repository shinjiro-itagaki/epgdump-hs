module Descriptor.LDT_Link (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),TOS(..),HasServiceID(..),HasOriginalNetworkID(..),HasUserDefined(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, TOS a) => Class a where
  original_service_id :: a -> Word16
  original_service_id = service_id
  descriptions :: a -> [LDT_LinkDesc]

data Data = MkData {
  _descriptor_tag      :: Word8,
  _descriptor_length   :: Word8,
  _original_service_id :: Word16,
  _descriptions        :: [LDT_LinkDesc],
  _transport_stream_id :: Word16,
  _original_network_id :: Word16,
  _service_id          :: Word16
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


data LDT_LinkDesc = MkLDT_LinkDesc {
  description_id :: Word16,
--  reserved_future_use :: Word8,
  description_type :: Word8,
  _LDT_LinkDesc_user_defined :: Word8
  }

instance HasUserDefined LDT_LinkDesc where
  user_defined = _LDT_LinkDesc_user_defined
