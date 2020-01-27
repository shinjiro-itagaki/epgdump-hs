module Descriptor.Link.ContentInfo (
  Class(..)
  ,Data
  ,mk
  ) where

import Utils
import qualified Utils.EmptyExist as EmptyExist
import qualified Descriptor.Link.ServiceInfo as ServiceInfo

class (ServiceInfo.Class a, Show a) => Class a where
  content_info :: a -> Data
  
  content_id :: a -> Word32
  content_id = content_id . content_info

data Data = MkData {
  _service_info :: ServiceInfo.Data,
  _content_id :: Word32
  } deriving (Show,Eq) -- 0x04

mk :: ServiceInfo.Data -> Word32 -> Data
mk = MkData

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty

instance ServiceInfo.Class Data where
  service_info = _service_info
  
instance Class Data where
  content_info x = x
  content_id = _content_id
