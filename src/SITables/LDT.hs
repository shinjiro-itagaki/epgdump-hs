module SITables.LDT (
  Data,
  Class(..)
  ) where
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import qualified SITables.Base as Base
import qualified SITables.LDT.Item as Item
import qualified Parser.Result as Result
import Data.Vector(Vector,toList,empty,snoc)
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs
import Utils

class (Base.Class a, Header1.Class a, Header2.Class a, ServiceInfo.Class a) => Class a where
--  original_network_id :: a -> Word16
  original_service_id :: a -> Word16
--  transport_stream_id :: a -> Word16
  
data Data = MkData {
  _header1             :: Header1.Data,
  _original_service_id :: Word16,
  _header2             :: Header2.Data,
  _transport_stream_id :: Word16,
  _original_network_id :: Word16,
  _items               :: Vector Item.Data,
  _footer              :: Footer.Data
  } deriving (Show)
  
instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance Footer.Class Data where
  footer = _footer

instance ServiceInfo.Class Data where
  original_network_id = _original_network_id  
  service_id          = _original_service_id
  transport_stream_id = _transport_stream_id  

instance Class Data where
--  original_network_id = _original_network_id  
  original_service_id = _original_service_id
--  transport_stream_id = _transport_stream_id

instance EmptyExist.Class Data where
  mkEmpty = MkData {
  _header1             = mkEmpty,
  _original_service_id = mkEmpty,
  _header2             = mkEmpty,
  _transport_stream_id = mkEmpty,
  _original_network_id = mkEmpty,
  _items               = Data.Vector.empty,
  _footer              = mkEmpty
  }

-- _parseIOFlow2 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow2 fh init =
--   getBitsIO_M fh [
--   (16, (\(v,d) -> d { _original_network_id = fromWord64 v}))
--   ] init

-- _parseIOFlow4 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow4 fh init = do
--   getBitsIO_M fh [
--     (16, (\(v,d) -> d { _transport_stream_id = fromWord64 v})),
--     (16, (\(v,d) -> d { _original_network_id = fromWord64 v}))
--     ] init

-- _parseIOFlow5_items :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow5_items bh init = Items.gather addItem' (Base.section_length_without_crc init) bh init
--   where
--     addItem' :: Data -> Item.Data -> Data
--     addItem' x item = x {_items = (snoc (_items x) item)  }

instance Base.Class Data where
  footer = Just . _footer
--   parseIOFlowAfterHeader1 =
--     flowStart
--     |>>= _parseIOFlow2
--     |>>= Header2.parseFlow
--     |>>= _parseIOFlow4
--     |>>= _parseIOFlow5_items
--     |>>= Footer.parseFlow
  

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0025]
  table_ids _ = [0xC7]

instance FromByteString.Class Data where
