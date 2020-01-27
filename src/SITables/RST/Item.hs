{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.RST.Item(
  Data,
  Class(..),
  ) where

import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import qualified Descriptor
import Data.Vector(Vector,toList,empty)
import qualified Utils.FromByteString as FromByteString
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Descriptor.Link.EventInfo as EventInfo
import qualified Descriptor.EventInfo as Info
import qualified Utils.EmptyExist as EmptyExist
import Utils

class (EventInfo.Class a) => Class a where
  reserved_future_use :: a -> Word8 -- 5
  running_status      :: a -> Word8 -- 3

data Data = MkData {
  _event_info          :: EventInfo.Data,
  _reserved_future_use :: Word8, -- 5
  _running_status      :: Word8 -- 3
  } deriving (Show)

instance ServiceInfo.Class Data where
  service_info = ServiceInfo.service_info . _event_info
  transport_stream_id = ServiceInfo.id0 -- 通常とは逆なので書き換え
  original_network_id = ServiceInfo.id1 -- 通常とは逆なので書き換え

instance Info.Class Data where
  service_id = ServiceInfo.service_id . _event_info
  event_id   = EventInfo.event_id    . _event_info
 
instance EventInfo.Class Data where
  event_info = _event_info
  
instance Class Data where
  reserved_future_use = _reserved_future_use
  running_status      = _running_status
  
instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let ((event_info,w8),rest) = fromByteStringWithRest bs
        reserved_future_use    = (w8 .&. 0xF8) `shiftR` 3 -- 5
        running_status         = (w8 .&. 0x07)            -- 3
        d = MkData {
          _event_info          = event_info,
          _reserved_future_use = reserved_future_use,
          _running_status      = running_status
          }
    in (d,rest)
