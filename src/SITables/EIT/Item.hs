{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.EIT.Item(
  Data,
  Class(..),
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import Common(HasOriginalNetworkID(..),EmptyExist(..),PID,TableID,BytesHolderIO(..))
import Descriptor(HasServiceID(..))
import Parser(HasParser(..),FromWord64(..),ParseResult(..))
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty)
import SITables.Items(Element(..))

class (HasDescriptors a) => Class a where
  event_id                :: a -> Word16
  start_time              :: a -> Word64
  duration                :: a -> Word32
  running_status          :: a -> Word8
  free_CA_mode            :: a -> Bool
  descriptors_loop_length :: a -> Word16

data Data = MkData {
  _event_id                :: Word16,
  _start_time              :: Word64,
  _duration                :: Word32,
  _running_status          :: Word8,
  _free_CA_mode            :: Bool,
  _descriptors_loop_length :: Word16,
  _descriptors             :: Vector Descriptor.Data
  }

instance HasDescriptors Data where
  descriptors = toList . _descriptors

instance Class Data where
  event_id                = _event_id 
  start_time              = _start_time
  duration                = _duration
  running_status          = _running_status
  free_CA_mode            = _free_CA_mode
  descriptors_loop_length = _descriptors_loop_length

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty

_parseIOFlow :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = do
  getBitsIO_M fh [
    (16, (\(v,d) -> d { _event_id                = fromWord64 v})),
    (40, (\(v,d) -> d { _start_time              = fromWord64 v})),
    (24, (\(v,d) -> d { _duration                = fromWord64 v})),
    ( 3, (\(v,d) -> d { _running_status          = fromWord64 v})),
    ( 1, (\(v,d) -> d { _free_CA_mode            = fromWord64 v})),
    (12, (\(v,d) -> d { _descriptors_loop_length = fromWord64 v}))
    ] init

instance HasParser Data where
  parseIOFlow = 
    flowStart |>>= _parseIOFlow
    
instance Element Data where
