{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.LDT.Item(
  Data,
  Class(..),
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import Common(EmptyExist(..),PID,TableID)
import qualified BytesReader.HolderIO as HolderIO
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty)
import SITables.Items(Element(..))

class (HasDescriptors a) => Class a where
  description_id          :: a -> Word16
  descriptors_loop_length :: a -> Word16
  reserved_future_use     :: a -> Word16

data Data = MkData {
  _description_id          :: Word16,
  _reserved_future_use     :: Word16, -- 12
  _descriptors_loop_length :: Word16,
  _descriptors             :: Vector Descriptor.Data
  }

instance HasDescriptors Data where
  descriptors = toList . _descriptors

instance Class Data where
  description_id          = _description_id 
  descriptors_loop_length = _descriptors_loop_length
  reserved_future_use     = _reserved_future_use
  
instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty Data.Vector.empty

_parseIOFlow1 :: (HolderIO.Class bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow1 fh init = do
  getBitsIO_M fh [
    (16, (\(v,d) -> d { _description_id          = fromWord64 v})),
    (12, (\(v,d) -> d { _reserved_future_use     = fromWord64 v})),
    (12, (\(v,d) -> d { _descriptors_loop_length = fromWord64 v}))
    ] init

instance Parser.Class Data where
  parseIOFlow = 
    flowStart
    |>>= _parseIOFlow1
    
instance Element Data where
