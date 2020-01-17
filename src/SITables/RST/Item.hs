{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.RST.Item(
  Data,
  Class(..),
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import Common(HasOriginalNetworkID(..),EmptyExist(..),PID,TableID,BytesHolderIO(..),HasServiceID(..))
--import Descriptor(HasServiceID(..))
import Parser(HasParser(..),FromWord64(..),ParseResult(..))
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty)
import SITables.Items(Element(..))

class (HasOriginalNetworkID a, HasServiceID a) => Class a where
  transport_stream_id :: a -> Word16
--  original_network_id :: a -> Word16
--  service_id          :: a -> Word16
  event_id            :: a -> Word16
  reserved_future_use :: a -> Word8 -- 5
  running_status      :: a -> Word8 -- 3

data Data = MkData {
  _transport_stream_id :: Word16,
  _original_network_id :: Word16,
  _service_id          :: Word16,
  _event_id            :: Word16,
  _reserved_future_use :: Word8, -- 5
  _running_status      :: Word8 -- 3
  }
  
instance Class Data where
  transport_stream_id = _transport_stream_id
--  original_network_id = _original_network_id
--  service_id          = _service_id
  event_id            = _event_id
  reserved_future_use = _reserved_future_use
  running_status      = _running_status
  
instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty

_parseIOFlow :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = do
  getBitsIO_M fh [
    (16, (\(v,d) -> d { _transport_stream_id = fromWord64 v})),
    (16, (\(v,d) -> d { _original_network_id = fromWord64 v})),
    (16, (\(v,d) -> d { _service_id          = fromWord64 v})),
    (16, (\(v,d) -> d { _event_id            = fromWord64 v})),
    ( 5, (\(v,d) -> d { _reserved_future_use = fromWord64 v})),
    ( 3, (\(v,d) -> d { _running_status      = fromWord64 v}))
    ] init

instance HasParser Data where
  parseIOFlow = 
    flowStart |>>= _parseIOFlow
    
instance Element Data where

instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance HasServiceID Data where
  service_id = _service_id
