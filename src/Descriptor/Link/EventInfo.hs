module Descriptor.Link.EventInfo (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)

import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Descriptor.EventInfo as Info

class (ServiceInfo.Class a, Info.Class a) => Class a where

data Data = MkData {
  _original_network_id :: Word16,
  _transport_stream_id :: Word16,
  _service_id :: Word16,
  _event_id :: Word16
  } deriving (Show) -- 0x02

instance ServiceInfo.Class Data where
  original_network_id = _original_network_id 
  transport_stream_id = _transport_stream_id
  service_id          = _service_id
  
instance Info.Class Data where
  event_id   = _event_id
  service_id = _service_id

instance Class Data where
