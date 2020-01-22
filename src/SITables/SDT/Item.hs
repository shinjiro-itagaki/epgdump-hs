{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SITables.SDT.Item(
  Data,
  Class(..),
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import Common(EmptyExist(..),PID,TableID)
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty)
import SITables.Items(Element(..))
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser

class (HasDescriptors a) => Class a where
  service_id                 :: a -> Word16
  reserved_future_use        :: a -> Word8
  eit_user_defined_flags     :: a -> Word8
  eit_schedule_flag          :: a -> Bool
  eit_present_following_flag :: a -> Bool
  running_status             :: a -> Word8
  free_CA_mode               :: a -> Bool
  descriptors_loop_length    :: a -> Word16

data Data = MkData {
  _service_id                 :: Word16,
  _reserved_future_use        :: Word8,
  _eit_user_defined_flags     :: Word8,
  _eit_schedule_flag          :: Bool,
  _eit_present_following_flag :: Bool,
  _running_status             :: Word8,
  _free_CA_mode               :: Bool,
  _descriptors_loop_length    :: Word16,
  _descriptors                :: Vector Descriptor.Data
  } deriving (Show)

instance HasDescriptors Data where
  descriptors = toList . _descriptors

instance Class Data where
  service_id = _service_id  
  reserved_future_use        = _reserved_future_use
  eit_user_defined_flags     = _eit_user_defined_flags     
  eit_schedule_flag          = _eit_schedule_flag
  eit_present_following_flag = _eit_present_following_flag
  running_status             = _running_status
  free_CA_mode               = _free_CA_mode
  descriptors_loop_length    = _descriptors_loop_length

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty

_parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = do
  getBitsIO_M fh [
    (16, (\(v,d) -> d { _service_id                 = fromWord64 v})),
    ( 3, (\(v,d) -> d { _reserved_future_use        = fromWord64 v})),
    ( 3, (\(v,d) -> d { _eit_user_defined_flags     = fromWord64 v})),
    ( 1, (\(v,d) -> d { _eit_schedule_flag          = fromWord64 v})),
    ( 1, (\(v,d) -> d { _eit_present_following_flag = fromWord64 v})),
    ( 3, (\(v,d) -> d { _running_status             = fromWord64 v})),    
    ( 1, (\(v,d) -> d { _free_CA_mode               = fromWord64 v})),
    (12, (\(v,d) -> d { _descriptors_loop_length    = fromWord64 v}))
    ] init

instance Parser.Class Data where
  parseIOFlow = 
    flowStart
    |>>= _parseIOFlow
    
instance Element Data where
