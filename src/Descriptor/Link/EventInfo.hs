module Descriptor.Link.EventInfo (
  Class(..)
  ,Data
  ) where

import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import Utils

class (ServiceInfo.Class a, FromByteString.Class a, Show a) => Class a where
  event_info :: a -> Data
  event_id :: a -> Word16
  event_id = event_id . event_info

data Data = MkData {
  _service_info :: ServiceInfo.Data,
  _event_id :: Word16
  } deriving (Show,Eq) -- 0x02

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty

instance ServiceInfo.Class Data where
  service_info = _service_info
  
instance FromByteString.Class Data where
  fromByteStringWithRest bs0 =
    let (service_info ,bs1) = fromByteStringWithRest bs0
        (event_id     ,bs2) = fromByteStringWithRest bs1
        d = MkData {
          _service_info = service_info,
          _event_id     = event_id
          }
    in (d, bs2)

instance Class Data where
  event_id = _event_id
