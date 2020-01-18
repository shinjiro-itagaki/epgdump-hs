module SITables.NIT(
  Data,
  Class(..)
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..),SITableIDs(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import Common(EmptyExist(..),PID,TableID,BytesHolderIO(..),TableID,PID,PIDs(..))
import qualified Descriptor
import qualified SITables.Base as Base
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import qualified Parser
import qualified SITables.NIT.Item as Item
import Data.Vector(Vector,toList,empty,snoc)
import FromWord64 hiding (Class)

import qualified Parser.Result as Result

class (Header1.Class a, Header2.Class a, HasDescriptors a) => Class a where
  network_id                   :: a -> Word16
  reserved_future_use1         :: a -> Word8  
  network_descriptors_length   :: a -> Word16
  reserved_future_use2         :: a -> Word8
  transport_stream_loop_length :: a -> Word16
  transport_streams            :: a -> [Item.Data]

data Data = MkData {
  _header1                      :: Header1.Data,
  _network_id                   :: Word16,
  _header2                      :: Header2.Data,
  _reserved_future_use1         :: Word8,
  _network_descriptors_length   :: Word16,
  _descriptors                  :: Vector Descriptor.Data,
  _reserved_future_use2         :: Word8,
  _transport_stream_loop_length :: Word16,
  _items                        :: Vector Item.Data,
  _footer                       :: Footer.Data
  }

instance Header1.Class Data where
  setHeader1 d h = d { _header1 = h }
  header1 = _header1
  
instance Header2.Class Data where
  setHeader2 d h = d { _header2 = h }
  header2 = _header2

instance Footer.Class Data where
  setFooter d x = d { _footer = x }  
  footer = _footer
  
instance HasDescriptors Data where
  descriptors = toList . _descriptors

instance Class Data where
  network_id                   = _network_id
  reserved_future_use1         = _reserved_future_use1
  network_descriptors_length   = _network_descriptors_length
  reserved_future_use2         = _reserved_future_use2
  transport_stream_loop_length = _transport_stream_loop_length
  transport_streams            = toList . _items

instance EmptyExist Data where
  mkEmpty = MkData {
    _header1                      = mkEmpty,
    _network_id                   = mkEmpty,
    _header2                      = mkEmpty,
    _reserved_future_use1         = mkEmpty,
    _network_descriptors_length   = mkEmpty,
    _descriptors                  = Data.Vector.empty,
    _reserved_future_use2         = mkEmpty,
    _transport_stream_loop_length = mkEmpty,
    _items                        = Data.Vector.empty,
    _footer                       = mkEmpty
    }

instance Parser.Class Data where

instance SITableIDs Data where
  pids      _ = MkPIDs [0x0010]
  table_ids _ = [0x40,0x41]

_parseIOFlow1 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow1 fh init =
  getBitsIO_M fh [
  (16, (\(v,d) -> d { _network_id = fromWord64 v}))
  ] init

_parseIOFlow2 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow2 fh init =
  getBitsIO_M fh [
  ( 4, (\(v,d) -> d { _reserved_future_use1       = fromWord64 v})),
  (12, (\(v,d) -> d { _network_descriptors_length = fromWord64 v}))
  ] init

_parseIOFlow3 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow3 fh init = return (Result.Parsed init, fh) -- todo descriptors

_parseIOFlow4 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow4 fh init = 
  getBitsIO_M fh [
  ( 4, (\(v,d) -> d { _reserved_future_use2         = fromWord64 v})),
  (12, (\(v,d) -> d { _transport_stream_loop_length = fromWord64 v}))
  ] init

_parseIOFlow5 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow5 fh init = return (Result.Parsed init, fh) -- todo items

instance Base.Class Data where
  footer = Just . _footer
  parseIOFlowAfterHeader1 =
    flowStart
    |>>= _parseIOFlow1
    |>>= Header2.parseFlow
    |>>= _parseIOFlow2
    |>>= _parseIOFlow3
    |>>= _parseIOFlow4
    |>>= _parseIOFlow5    
    |>>= Footer.parseFlow    
