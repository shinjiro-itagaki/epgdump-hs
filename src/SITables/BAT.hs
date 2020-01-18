module SITables.BAT (
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
import qualified SITables.BAT.Item as Item
import Data.Vector(Vector,toList,empty,snoc)
import Parser(HasParser(..),FromWord64(..),ParseResult(..),flowStart,(|>>=))

class (Base.Class a, Header2.Class a, Footer.Class a, HasDescriptors a) => Class a where
  bouquet_id                   :: a -> Word16
  reserved_future_use1         :: a -> Word8
  bouquet_descriptors_length   :: a -> Word16
  reserved_future_use2         :: a -> Word8  
  transport_stream_loop_length :: a -> Word16
  items                        :: a -> [Item.Data]

data Data = MkData {
  _header1                      :: Header1.Data,
  _bouquet_id                   :: Word16,
  _header2                      :: Header2.Data,
  _reserved_future_use1         :: Word8,
  _bouquet_descriptors_length   :: Word16, 
  _descriptors                  :: Vector Descriptor.Data,
  _reserved_future_use2         :: Word8,  
  _transport_stream_loop_length :: Word16,
  _items                        :: Vector Item.Data,
  _footer                       :: Footer.Data
  }

instance Header1.Class Data where
  header1 = _header1
  setHeader1 x h = x {_header1 = h}  
  
instance Header2.Class Data where
  header2 = _header2
  setHeader2 x h = x {_header2 = h}

instance Footer.Class Data where
  footer = _footer
  setFooter x y = x {_footer = y}
  
instance HasDescriptors Data where
  descriptors = toList . _descriptors

instance Class Data where
  bouquet_id                   = _bouquet_id
  reserved_future_use1         = _reserved_future_use1
  bouquet_descriptors_length   = _bouquet_descriptors_length
  reserved_future_use2         = _reserved_future_use2
  transport_stream_loop_length = _transport_stream_loop_length
  items                        = toList . _items

instance EmptyExist Data where
  mkEmpty = MkData {
    _header1                      = mkEmpty,
    _bouquet_id                   = mkEmpty,
    _header2                      = mkEmpty,
    _reserved_future_use1         = mkEmpty,
    _bouquet_descriptors_length   = mkEmpty,
    _descriptors                  = Data.Vector.empty,
    _reserved_future_use2         = mkEmpty,
    _transport_stream_loop_length = mkEmpty,
    _items                        = Data.Vector.empty,
    _footer                       = mkEmpty
  }

instance HasParser Data where

_parseIOFlow1 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow1 fh init =
  getBitsIO_M fh [
  (16, (\(v,d) -> d { _bouquet_id = fromWord64 v}))
  ] init

_parseIOFlow2 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow2 fh init =
  getBitsIO_M fh [
  ( 4, (\(v,d) -> d { _reserved_future_use1       = fromWord64 v})),
  (12, (\(v,d) -> d { _bouquet_descriptors_length = fromWord64 v}))
  ] init

_parseIOFlow3_descs :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow3_descs fh init = return (Parsed init, fh) -- todo descriptors

_parseIOFlow4 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow4 fh init = 
  getBitsIO_M fh [
  ( 4, (\(v,d) -> d { _reserved_future_use2         = fromWord64 v})),
  (12, (\(v,d) -> d { _transport_stream_loop_length = fromWord64 v}))
  ] init

_parseIOFlow5_items :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow5_items fh init = return (Parsed init, fh) -- todo items
  
  
instance Base.Class Data where
  footer  = Just . _footer
  parseIOFlowAfterHeader1 =
    flowStart
    |>>= _parseIOFlow1
    |>>= Header2.parseFlow
    |>>= _parseIOFlow2
    |>>= _parseIOFlow3_descs
    |>>= _parseIOFlow4
    |>>= _parseIOFlow5_items
    |>>= Footer.parseFlow

instance SITableIDs Data where  
  pids      _ = MkPIDs [0x0011]
  table_ids _ = [0x4A]
