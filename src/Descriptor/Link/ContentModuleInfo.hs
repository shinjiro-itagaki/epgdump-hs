module Descriptor.Link.ContentModuleInfo (
  Class(..)
  ,Data
  ) where

import Utils
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Descriptor.Link.ContentInfo as ContentInfo
import qualified Descriptor.Link.ModuleInfo as ModuleInfo
import qualified Utils.EmptyExist as EmptyExist
  
class (ContentInfo.Class a) => Class a where
  content_info  :: a -> ContentInfo.Data
  component_tag :: a -> Word8
  module_id     :: a -> Word16
  
data Data = MkData {
  _content_info :: ContentInfo.Data,
  _component_tag :: Word8,
  _module_id :: Word16
  } deriving (Show,Eq) -- 0x05

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty

instance ServiceInfo.Class Data where
  service_info = ServiceInfo.service_info . _content_info
  
instance ContentInfo.Class Data where
  content_info =  _content_info

instance Class Data where
  component_tag       = _component_tag
  module_id           = _module_id

