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
import qualified Data.ByteString.Lazy as BS
import qualified Utils.FromByteString as FromByteString
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

instance Base.Class Data where
  footer = Just . _footer
  parseAfterHeader1 h bs =
    let ((footer,
          network_id,
          header2,
          w16),bs0) = fromByteStringWithRest bs
        reserved_future_use1       = toWord8 $ (w16 .&. 0xF000) `shiftR` 12
        network_descriptors_length =           (w16 .&. 0x0FFF)
        (bs1,bs2) = BS.splitAt (fromInteger $ toInteger network_descriptors_length) bs0
        descriptors = fromByteString bs1
        (w16_2,bs3) = fromByteStringWithRest bs2
        reserved_future_use2         = toWord8 $ (w16_2 .&. 0xF000) `shiftR` 12
        transport_stream_loop_length =           (w16_2 .&. 0x0FFF)
        (bs4,_) = BS.splitAt (fromInteger $ toInteger transport_stream_loop_length) bs3
        items = fromByteString bs4
        d = MkData {
          _header1                      = h,
          _network_id                   = network_id,
          _header2                      = header2,
          _reserved_future_use1         = reserved_future_use1,
          _network_descriptors_length   = network_descriptors_length,
          _descriptors                  = descriptors,
          _reserved_future_use2         = reserved_future_use2,
          _transport_stream_loop_length = transport_stream_loop_length,
          _items                        = items,
          _footer                       = footer
          }
    in Result.Parsed d
