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
  _service_info        :: ServiceInfo.Data,
  _event_id            :: Word16,
  _reserved_future_use :: Word8, -- 5
  _running_status      :: Word8 -- 3
  } deriving (Show)

instance ServiceInfo.Class Data where
  service_info = _service_info
  transport_stream_id = ServiceInfo.id0 -- 通常とは逆なので書き換え
  original_network_id = ServiceInfo.id1 -- 通常とは逆なので書き換え

instance Info.Class Data where
  service_id = ServiceInfo.service_id . _service_info 
  event_id   = _event_id

instance FromByteString.Class Data where
  
instance EventInfo.Class Data where
  
instance Class Data where
  reserved_future_use = _reserved_future_use
  running_status      = _running_status
  
instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty

-- _parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow fh init = do
--   getBitsIO_M fh [
--     -- (16, (\(v,d) -> d { _transport_stream_id = fromWord64 v})),
--     -- (16, (\(v,d) -> d { _original_network_id = fromWord64 v})),
--     -- (16, (\(v,d) -> d { _service_id          = fromWord64 v})),
--     (16, (\(v,d) -> d )),
--     (16, (\(v,d) -> d )),
--     (16, (\(v,d) -> d )),
--     (16, (\(v,d) -> d { _event_id            = fromWord64 v})),    
--     ( 5, (\(v,d) -> d { _reserved_future_use = fromWord64 v})),
--     ( 3, (\(v,d) -> d { _running_status      = fromWord64 v}))
--     ] init

-- instance Parser.Class Data where
--   parseIOFlow = 
--     flowStart |>>= _parseIOFlow
    
