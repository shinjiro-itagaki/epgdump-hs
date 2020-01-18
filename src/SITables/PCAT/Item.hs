{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.PCAT.Item(
  Data,
  Class(..),
  ) where

import qualified Schedule
import SITables.Common(HasDescriptors(..),SITableIDs(..))
import Data.Word(Word64, Word32, Word16, Word8)
import Common(EmptyExist(..),PID,TableID)
import BytesReader(Holder(..),HolderIO(..))
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty)
import SITables.Items(Element(..))

class (HasDescriptors a) => Class a where
  content_version             :: a -> Word16
  content_minor_version       :: a -> Word16
  version_indicator           :: a -> (Bool,Bool)
  reserved_future_use1        :: a -> (Bool,Bool)
  content_descriptor_length   :: a -> Word16
  reserved_future_use2        :: a -> Word8
  schedule_description_length :: a -> Word16
  schedules                   :: a -> [Schedule.Data]
  descriptors                 :: a -> [Descriptor.Data]

  
data Data = MkData {
  _content_version             :: Word16,
  _content_minor_version       :: Word16,
  _version_indicator           :: (Bool,Bool),
  _reserved_future_use1        :: (Bool,Bool),
  _content_descriptor_length   :: Word16,
  _reserved_future_use2        :: Word8,
  _schedule_description_length :: Word16,
  _schedules                   :: Vector Schedule.Data,
  _descriptors                 :: Vector Descriptor.Data
  }

instance HasDescriptors Data where
  descriptors = toList . _descriptors

instance Class Data where
  content_version             = _content_version
  content_minor_version       = _content_minor_version
  version_indicator           = _version_indicator
  reserved_future_use1        = _reserved_future_use1
  content_descriptor_length   = _content_descriptor_length
  reserved_future_use2        = _reserved_future_use2
  schedule_description_length = _schedule_description_length
  schedules                   = toList . _schedules

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty Data.Vector.empty

_parseIOFlow :: (HolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = do
  getBitsIO_M fh [
    (16, (\(v,d) -> d { _content_version                = fromWord64 v})),
    (16, (\(v,d) -> d { _content_minor_version          = fromWord64 v})),
    ( 2, (\(v,d) -> d { _version_indicator              = fromWord64 v})),
    ( 2, (\(v,d) -> d { _reserved_future_use1           = fromWord64 v})),
    (12, (\(v,d) -> d { _content_descriptor_length      = fromWord64 v})),
    ( 4, (\(v,d) -> d { _reserved_future_use2           = fromWord64 v})),
    (12, (\(v,d) -> d { _schedule_description_length    = fromWord64 v}))
    ] init

instance Parser.Class Data where
  parseIOFlow = 
    flowStart |>>= _parseIOFlow
    
instance Element Data where
