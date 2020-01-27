module SITables.NIT(
  Data,
  Class(..)
  ) where
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import qualified SITables.Base as Base
import qualified Parser.Result as Result
import qualified SITables.NIT.Item as Item
import Data.Vector(Vector,toList,empty,snoc)
import qualified Utils.SITableIDs as SITableIDs
import qualified Utils.EmptyExist as EmptyExist
import Utils

import qualified Utils.FromByteString as FromByteString

class (Header1.Class a, Header2.Class a) => Class a where
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
  } deriving (Show)

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance Footer.Class Data where
  footer = _footer
  
instance Class Data where
  network_id                   = _network_id
  reserved_future_use1         = _reserved_future_use1
  network_descriptors_length   = _network_descriptors_length
  reserved_future_use2         = _reserved_future_use2
  transport_stream_loop_length = _transport_stream_loop_length
  transport_streams            = toList . _items

instance EmptyExist.Class Data where
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

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0010]
  table_ids _ = [0x40,0x41]

-- _parseIOFlow1 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow1 fh init =
--   getBitsIO_M fh [
--   (16, (\(v,d) -> d { _network_id = fromWord64 v}))
--   ] init

-- _parseIOFlow2 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow2 fh init =
--   getBitsIO_M fh [
--   ( 4, (\(v,d) -> d { _reserved_future_use1       = fromWord64 v})),
--   (12, (\(v,d) -> d { _network_descriptors_length = fromWord64 v}))
--   ] init

-- _parseIOFlow3 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow3 fh init = return (Result.Parsed init, fh) -- todo descriptors

-- _parseIOFlow4 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow4 fh init = 
--   getBitsIO_M fh [
--   ( 4, (\(v,d) -> d { _reserved_future_use2         = fromWord64 v})),
--   (12, (\(v,d) -> d { _transport_stream_loop_length = fromWord64 v}))
--   ] init

-- _parseIOFlow5 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow5 fh init = return (Result.Parsed init, fh) -- todo items

instance Base.Class Data where
  footer = Just . _footer
--   parseIOFlowAfterHeader1 =
--     flowStart
--     |>>= _parseIOFlow1
--     |>>= Header2.parseFlow
--     |>>= _parseIOFlow2
--     |>>= _parseIOFlow3
--     |>>= _parseIOFlow4
--     |>>= _parseIOFlow5    
--     |>>= Footer.parseFlow    

instance FromByteString.Class Data where
