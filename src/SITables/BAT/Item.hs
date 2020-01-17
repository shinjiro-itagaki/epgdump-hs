{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.BAT.Item(
  Data,
  Class(..),
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import Common(HasOriginalNetworkID(..),EmptyExist(..),PID,TableID,BytesHolderIO(..))
import Parser(HasParser(..),FromWord64(..),ParseResult(..))
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty)
import SITables.Items(Element(..))

class (HasDescriptors a, HasOriginalNetworkID a) => Class a where
  transport_stream_id          :: a -> Word16
  reserved_future_use          :: a -> Word8
  transport_descriptors_length :: a -> Word16

data Data = MkData {
  _transport_stream_id          :: Word16,
  _original_network_id          :: Word16,
  _reserved_future_use          :: Word8,
  _transport_descriptors_length :: Word16,
  _descriptors                  :: Vector Descriptor.Data
  }

instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance HasDescriptors Data where
  descriptors = toList . _descriptors

instance Class Data where
  transport_stream_id          = _transport_stream_id
  reserved_future_use          = _reserved_future_use
  transport_descriptors_length = _transport_descriptors_length
  
