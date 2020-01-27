module SITables.PCAT (
  Data,
  Class(..)
  ) where
import qualified Utils.Schedule as Schedule
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import qualified SITables.Base as Base
import qualified Parser.Result as Result
import Utils
import Data.Vector(Vector,toList,empty,snoc,fromList)
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
  _header2    :: Header2.Data,
  _content_info :: ContentInfo.Data,
  _num_of_content_version :: Word8,
  _items                  :: Vector Item.Data,
  _footer                 :: Footer.Data
  } deriving (Show)

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance Footer.Class Data where
  footer = _footer

instance ServiceInfo.Class Data where
  service_info = ServiceInfo.service_info . _content_info

instance ContentInfo.Class Data where
  content_info = _content_info
  
instance Class Data where
  num_of_content_version = _num_of_content_version

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0022]
  table_ids _ = [0xC2]

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty mkEmpty
  
instance Base.Class Data where
  footer = Just . _footer  
  parseAfterHeader1 h bs =
    let (footer,bs0) = fromByteStringWithRest bs
        ((service_id,
          header2,
          transport_stream_id,
          original_network_id,
          content_id,
          num_of_content_version),bs1) = fromByteStringWithRest bs0
        items = fromList $ fromByteString_n num_of_content_version bs1
        service_info = ServiceInfo.mk original_network_id transport_stream_id service_id
        content_info = ContentInfo.mk service_info content_id
        d = MkData {
          _header1    = h,
          _content_info = content_info,
          _header2    = header2,
          _num_of_content_version = num_of_content_version,
          _items                  = items,
          _footer                 = footer
          }
    in Result.Parsed d
