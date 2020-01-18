{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.NBIT.Item(
  Data,
  Class(..),
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import Common(EmptyExist(..),PID,TableID)
import BytesReader(Holder(..),HolderIO(..))
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty,snoc)
import SITables.Items(Element(..))

class (HasDescriptors a) => Class a where
  information_id            :: a -> Word16
  information_type          :: a -> Word8
  description_body_location :: a -> (Bool,Bool)
  reserved_future_use1      :: a -> Word8
  user_defined              :: a -> Word8
  number_of_keys            :: a -> Word8
  keys                      :: a -> [Word16]
  reserved_future_use2      :: a -> Word8
  descriptors_loop_length   :: a -> Word16
  descriptors               :: a -> [Descriptor.Data]
  
data Data = MkData {
  _information_id            :: Word16,
  _information_type          :: Word8,
  _description_body_location :: (Bool,Bool),
  _reserved_future_use1      :: Word8, -- 2,
  _user_defined              :: Word8,
  _number_of_keys            :: Word8,
  _keys                      :: Vector Word16,
  _reserved_future_use2      :: Word8, -- 4
  _descriptors_loop_length   :: Word16,
  _descriptors               :: Vector Descriptor.Data
  }

instance Class Data where
  information_id            = _information_id
  information_type          = _information_type 
  description_body_location = _description_body_location
  reserved_future_use1      = _reserved_future_use1 
  user_defined              = _user_defined
  number_of_keys            = _number_of_keys
  keys                      = toList . _keys
  reserved_future_use2      = _reserved_future_use2
  descriptors_loop_length   = _descriptors_loop_length
  descriptors               = toList . _descriptors

instance HasDescriptors Data where
  descriptors = toList . _descriptors

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty mkEmpty mkEmpty Data.Vector.empty

_parseIOFlow1 :: (HolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow1 fh init = do
  getBitsIO_M fh [
    (16, (\(v,d) -> d { _information_id            = fromWord64 v})),
    ( 4, (\(v,d) -> d { _information_type          = fromWord64 v})),
    ( 2, (\(v,d) -> d { _description_body_location = fromWord64 v})),
    ( 2, (\(v,d) -> d { _reserved_future_use1      = fromWord64 v})),
    ( 8, (\(v,d) -> d { _user_defined              = fromWord64 v})),
    ( 8, (\(v,d) -> d { _number_of_keys            = fromWord64 v}))
    ] init
    
_parseIOFlow2 :: (HolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow2 fh init =
  let n = number_of_keys init
      list = map (\_ -> (16, (\(v,d) -> d { _keys = snoc (_keys d) $ fromWord64 v})) ) [1 .. n]
  in getBitsIO_M fh list init

_parseIOFlow3 :: (HolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow3 fh init = do
  getBitsIO_M fh [  
    ( 4, (\(v,d) -> d { _reserved_future_use2      = fromWord64 v})),
    (12, (\(v,d) -> d { _descriptors_loop_length   = fromWord64 v}))
    ] init

instance Parser.Class Data where
  parseIOFlow = 
    flowStart
    |>>= _parseIOFlow1
    |>>= _parseIOFlow2
    |>>= _parseIOFlow3
    
instance Element Data where
