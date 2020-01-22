{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.BIT.Item(
  Data,
  Class(..),
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import Common(EmptyExist(..),PID,TableID)
import qualified BytesReader.Base as BytesReaderBase
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty)
import SITables.Items(Element(..))

class (HasDescriptors a) => Class a where
  broadcaster_id                 :: a -> Word8
  reserved_future_use            :: a -> Word8
  broadcaster_descriptors_length :: a -> Word16
  descriptors                    :: a -> [Descriptor.Data]
  
data Data = MkData {
  _broadcaster_id                 :: Word8,
  _reserved_future_use            :: Word8,
  _broadcaster_descriptors_length :: Word16,
  _descriptors                    :: Vector Descriptor.Data
  } deriving (Show)

instance HasDescriptors Data where
  descriptors = toList . _descriptors

instance Class Data where
  broadcaster_id                 = _broadcaster_id
  reserved_future_use            = _reserved_future_use
  broadcaster_descriptors_length = _broadcaster_descriptors_length

instance EmptyExist Data where
  mkEmpty = MkData {
  _broadcaster_id                 = mkEmpty,
  _reserved_future_use            = mkEmpty,
  _broadcaster_descriptors_length = mkEmpty,
  _descriptors                    = Data.Vector.empty
  }

_parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = do
  getBitsIO_M fh [
    ( 8, (\(v,d) -> d { _broadcaster_id = fromWord64 v})),
    ( 4, (\(v,d) -> d { _reserved_future_use = fromWord64 v})),
    (12, (\(v,d) -> d { _broadcaster_descriptors_length = fromWord64 v}))
    ] init

instance Parser.Class Data where
  parseIOFlow = 
    flowStart |>>= _parseIOFlow
    
instance Element Data where
