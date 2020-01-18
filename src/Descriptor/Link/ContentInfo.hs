module Descriptor.Link.ContentInfo (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Link.ServiceInfo as ServiceInfo

class (ServiceInfo.Class a) => Class a where
  content_id :: a -> Word32

data Data = MkData {
  _original_network_id :: Word16,
  _transport_stream_id :: Word16,
  _service_id :: Word16,
  _content_id :: Word32
  } -- 0x04

instance ServiceInfo.Class Data where
  original_network_id = _original_network_id 
  transport_stream_id = _transport_stream_id
  service_id          = _service_id
  
instance Class Data where
  content_id = _content_id
