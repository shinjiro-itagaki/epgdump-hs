module Descriptor.Link.ServiceInfo (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)

class Class a where
  original_network_id :: a -> Word16
  transport_stream_id :: a -> Word16
  service_id          :: a -> Word16
  
data Data = MkData {
  _original_network_id :: Word16,
  _transport_stream_id :: Word16,
  _service_id :: Word16
  } -- 0x01

instance Class Data where
  original_network_id = _original_network_id 
  transport_stream_id = _transport_stream_id
  service_id          = _service_id

