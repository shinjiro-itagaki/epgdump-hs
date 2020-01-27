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
import qualified Data.ByteString.Lazy as BS
import qualified Utils.FromByteString as FromByteString

class Class a where
  transport_stream_id          :: a -> Word16
  original_network_id          :: a -> Word16
  reserved_future_use          :: a -> Word8
  transport_descriptors_length :: a -> Word16
  descriptors                  :: a -> [Descriptor.Data]
  
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
  descriptors                  = toList . _descriptors

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let ((transport_stream_id, original_network_id, w16),bs0) = fromByteStringWithRest bs
        reserved_future_use          = toWord8 $ (w16 .&. 0xF000) `shiftR` 12 -- 4
        transport_descriptors_length =           (w16 .&. 0x0FFF) -- 12
        (bs1,rest)                   = BS.splitAt (fromInteger $ toInteger transport_descriptors_length) bs0
        descriptors                  = fromByteString bs1
        d = MkData {
          _transport_stream_id          = transport_stream_id,
          _original_network_id          = original_network_id,
          _reserved_future_use          = reserved_future_use,
          _transport_descriptors_length = transport_descriptors_length,
          _descriptors                  = descriptors
          }
    in (d, rest)
