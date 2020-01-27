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

instance Base.Class Data where
  footer = Just . _footer
  parseAfterHeader1 h bs =
    let (footer,bs0) = fromByteStringWithRest bs
        (original_service_id,
         header2,
         transport_stream_id,
         original_network_id,
         items) = fromByteString bs0
        d = MkData {
          _header1             = h,
          _original_service_id = original_service_id,
          _header2             = header2,
          _transport_stream_id = transport_stream_id,
          _original_network_id = original_network_id,
          _items               = items,
          _footer              = footer
          }
    in Result.Parsed d

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0025]
  table_ids _ = [0xC7]

instance FromByteString.Class Data where
