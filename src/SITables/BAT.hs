module SITables.BAT (
  Data,
  Class(..)
  ) where
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import qualified SITables.Base as Base
import qualified SITables.BAT.Item as Item
import Data.Vector(Vector,toList,empty,snoc)
import qualified Parser.Result as Result
import Utils
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs

class (Base.Class a, Header2.Class a, Footer.Class a) => Class a where
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
  } deriving (Show)

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance Footer.Class Data where
  footer = _footer
  
instance Class Data where
  bouquet_id                   = _bouquet_id
  reserved_future_use1         = _reserved_future_use1
  bouquet_descriptors_length   = _bouquet_descriptors_length
  reserved_future_use2         = _reserved_future_use2
  transport_stream_loop_length = _transport_stream_loop_length
  items                        = toList . _items

instance EmptyExist.Class Data where
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

instance Base.Class Data where
  parseAfterHeader1 h bs =
    let (bouquet_id,bs0)             = fromByteStringWithRest bs
        (header2,bs1)                = fromByteStringWithRest bs0
        (w16,bs2)                    = fromByteStringWithRest bs1
        reserved_future_use1         = toWord8 $ (`shiftR` 12) $ w16 .&. 0xF000
        bouquet_descriptors_length   = w16 .&. 0x0FFF
        (w16',bs3)                   = fromByteStringWithRest bs2
        reserved_future_use2         = toWord8 $ (`shiftR` 12) $ w16' .&. 0xF000
        transport_stream_loop_length = w16' .&. 0x0FFF
        (footer,rest)                = fromByteStringWithRest bs3
        d = MkData {
          _header1                      = h,
          _bouquet_id                   = bouquet_id,
          _header2                      = header2,
          _reserved_future_use1         = reserved_future_use1,
          _bouquet_descriptors_length   = bouquet_descriptors_length,
          _descriptors                  = Data.Vector.empty,
          _reserved_future_use2         = reserved_future_use2,
          _transport_stream_loop_length = transport_stream_loop_length,
          _items                        = Data.Vector.empty,
          _footer                       = footer
          }
    in Result.Parsed d


instance SITableIDs.Class Data where  
  pids      _ = MkPIDs [0x0011]
  table_ids _ = [0x4A]
