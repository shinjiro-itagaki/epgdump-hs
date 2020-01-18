module Descriptor.Linkage (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.Link.ServiceInfo as ServiceInfo

class (Base.Class a, ServiceInfo.Class a) => Class a where
--  transport_stream_id :: a -> Word16
--  original_network_id :: a -> Word16
--  service_id          :: a -> Word16
  linkage_type        :: a -> Word8
  private_data_bytes  :: a -> ByteString

data Data = MkData {
  _header              :: Header.Data,
  _linkage_type        :: Word8, -- 8
  _transport_stream_id :: Word16, -- 16
  _original_network_id :: Word16, -- 16
  _service_id          :: Word16, -- 16
  _private_data_bytes  :: ByteString -- [8]
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance ServiceInfo.Class Data where  
  transport_stream_id  = _transport_stream_id
  original_network_id  = _original_network_id
  service_id           = _service_id

instance Class Data where
  linkage_type       = _linkage_type
  private_data_bytes = _private_data_bytes
