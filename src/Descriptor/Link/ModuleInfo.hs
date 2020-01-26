module Descriptor.Link.ModuleInfo (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString,EmptyExist(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Descriptor.Link.EventInfo   as EventInfo
import qualified Utils.FromByteString as FromByteString

class (EventInfo.Class a) => Class a where
  component_tag :: a -> Word8
  module_id     :: a -> Word16
  
data Data  = MkData {
  _event_info :: EventInfo.Data,
  _component_tag :: Word8,
  _module_id :: Word16
  } deriving (Show,Eq) -- 0x03

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty

instance FromByteString.Class Data where

instance ServiceInfo.Class Data where
  service_info = ServiceInfo.service_info . _event_info

instance EventInfo.Class Data where
  event_info = _event_info
  
instance Class Data where
  component_tag = _component_tag
  module_id     = _module_id

