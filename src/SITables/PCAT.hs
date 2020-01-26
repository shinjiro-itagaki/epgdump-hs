module SITables.PCAT (
  Data,
  Class(..)
  ) where
import qualified Schedule
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import qualified SITables.Base as Base
import qualified Parser.Result as Result
import Utils
import Data.Vector(Vector,toList,empty,snoc)
import qualified SITables.PCAT.Item as Item
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Descriptor.Link.ContentInfo as ContentInfo
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs
import qualified Utils.FromByteString as FromByteString

class (Header1.Class a, Header2.Class a, ContentInfo.Class a) => Class a where
  num_of_content_version :: a -> Word8

data Data = MkData {
  _header1    :: Header1.Data,
  _service_id :: Word16,
  _header2    :: Header2.Data,
  _transport_stream_id    :: Word16,
  _original_network_id    :: Word16,
  _content_id             :: Word32,
  _num_of_content_version :: Word8,
  _items                  :: Vector Item.Data,
  _footer                 :: Footer.Data
  } deriving (Show)

instance Header1.Class Data where
  setHeader1 d h = d {_header1 = h }
  header1 = _header1
  
instance Header2.Class Data where
  setHeader2 d h = d {_header2 = h }
  header2 = _header2

instance Footer.Class Data where
  footer = _footer
  setFooter x y = x {_footer = y}

instance ServiceInfo.Class Data where
  original_network_id = _original_network_id
  service_id          = _service_id
  transport_stream_id = _transport_stream_id

instance ContentInfo.Class Data where  
  content_id          = _content_id
  
instance Class Data where
  num_of_content_version = _num_of_content_version

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0022]
  table_ids _ = [0xC2]

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty mkEmpty

-- _parseIOFlow1 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow1 fh init = do
--   getBitsIO_M fh [
--     (16, (\(v,d) -> d { _service_id = fromWord64 v}))
--     ] init

-- _parseIOFlow2 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow2 fh init = do
--   getBitsIO_M fh [
--     (16, (\(v,d) -> d { _transport_stream_id    = fromWord64 v})),
--     (16, (\(v,d) -> d { _original_network_id    = fromWord64 v})),
--     (32, (\(v,d) -> d { _content_id             = fromWord64 v})),
--     ( 8, (\(v,d) -> d { _num_of_content_version = fromWord64 v}))
--     ] init
  
instance Base.Class Data where
  footer = Just . _footer
--   parseIOFlowAfterHeader1 =
--     flowStart
--     |>>= _parseIOFlow1
--     |>>= Header2.parseFlow
--     |>>= _parseIOFlow2
--     |>>= Footer.parseFlow    

instance FromByteString.Class Data where
