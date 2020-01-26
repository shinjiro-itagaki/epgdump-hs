{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.BAT.Item(
  Data,
  Class(..),
  ) where

import qualified Descriptor
import Data.Vector(Vector,toList,empty)
import Utils

class Class a where
  original_network_id          :: a -> Word16
  transport_stream_id          :: a -> Word16
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
  original_network_id          = _original_network_id  
  transport_stream_id          = _transport_stream_id
  reserved_future_use          = _reserved_future_use
  transport_descriptors_length = _transport_descriptors_length
