module SITables.SDT(
  Data,
  Class(..),
  pids, table_ids
  ) where
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import qualified SITables.Base as Base
import qualified Parser.Result as Result
import qualified SITables.Footer as Footer
import qualified SITables.SDT.Item as Item
import Data.Vector(Vector,toList,empty,snoc)
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs
import Utils

class (Header1.Class a, Header2.Class a, Footer.Class a) => Class a where
  original_network_id :: a -> Word16
  transport_stream_id :: a -> Word16
  reserved_future_use :: a -> Word8
  
data Data = MkData {
  _header1             :: Header1.Data,
  _transport_stream_id :: Word16,
  _header2             :: Header2.Data,
  _original_network_id :: Word16,
  _reserved_future_use :: Word8,
  _items               :: Vector Item.Data,
  _footer              :: Footer.Data
  } deriving (Show)

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty empty mkEmpty

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0011]
  table_ids _ = [0x42,0x46]

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance Footer.Class Data where
  footer = _footer

instance Class Data where
  original_network_id = _original_network_id
  transport_stream_id = _transport_stream_id
  reserved_future_use = _reserved_future_use

  
instance Base.Class Data where
  footer = Just . _footer
  parseAfterHeader1 h bs =
    let (footer,bs0) = fromByteStringWithRest bs
        (transport_stream_id, header2, original_network_id, reserved_future_use, items) = fromByteString bs0
        d = MkData {
          _header1             = h,
          _transport_stream_id = transport_stream_id,
          _header2             = header2,
          _original_network_id = original_network_id,
          _reserved_future_use = reserved_future_use,
          _items               = items,
          _footer              = footer
          }
    in Result.Parsed d
