{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.NIT.Item(
  Data,
  Class(..),
  ) where

import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import Utils
import qualified Descriptor
import Data.Vector(Vector,toList,empty)
import qualified Utils.EmptyExist as EmptyExist

class Class a where
  transport_stream_id          :: a -> Word16
  original_network_id          :: a -> Word16
  reserved_future_use          :: a -> Word8
  transport_descriptors_length :: a -> Word16
  
data Data = MkData {
  _transport_stream_id          :: Word16,
  _original_network_id          :: Word16,
  _reserved_future_use          :: Word8,
  _transport_descriptors_length :: Word16,
  _descriptors                  :: Vector Descriptor.Data
  } deriving (Show)

instance Class Data where
  transport_stream_id          = _transport_stream_id
  original_network_id          = _original_network_id
  reserved_future_use          = _reserved_future_use
  transport_descriptors_length = _transport_descriptors_length

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty

-- _parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow fh init = do
--   getBitsIO_M fh [
--     (16, (\(v,d) -> d { _transport_stream_id = fromWord64 v})),
--     (16, (\(v,d) -> d { _original_network_id = fromWord64 v})),
--     ( 4, (\(v,d) -> d { _reserved_future_use = fromWord64 v})),
--     (12, (\(v,d) -> d { _transport_descriptors_length = fromWord64 v}))
--     ] init

-- instance Parser.Class Data where
--   parseIOFlow = 
--     flowStart |>>= _parseIOFlow
    
