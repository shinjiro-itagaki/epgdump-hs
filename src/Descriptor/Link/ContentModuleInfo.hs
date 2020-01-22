module Descriptor.Link.ContentModuleInfo (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Descriptor.Link.ContentInfo as ContentInfo
import qualified Descriptor.Link.ModuleInfo as ModuleInfo

class (ContentInfo.Class a) => Class a where
  component_tag :: a -> Word8
  module_id     :: a -> Word16
  
data Data = MkData {
  _original_network_id :: Word16,
  _transport_stream_id :: Word16,
  _service_id :: Word16,
  _content_id :: Word32,
  _component_tag :: Word8,
  _module_id :: Word16
  } deriving (Show) -- 0x05

instance ServiceInfo.Class Data where
  original_network_id = _original_network_id 
  transport_stream_id = _transport_stream_id
  service_id          = _service_id
  
instance ContentInfo.Class Data where
  content_id          = _content_id  

instance Class Data where
  component_tag       = _component_tag
  module_id           = _module_id

