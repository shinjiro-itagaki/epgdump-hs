module SITables.BIT (
  Data,
  Class(..),
  pids,
  table_ids
  ) where
import qualified Utils.Schedule
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import qualified SITables.Base as Base
import qualified Parser.Result as Result
import Data.Vector(Vector,toList,empty,snoc)
import qualified SITables.BIT.Item as Item
import qualified Data.ByteString.Lazy as BS
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs
import Utils

class (Base.Class a, Header2.Class a, Footer.Class a) => Class a where
  original_network_id      :: a -> Word16
  reserved_future_use      :: a -> Word8 
  broadcast_view_propriety :: a -> Bool
  first_descriptors_length :: a -> BytesLen
  descriptors              :: a -> [Descriptor.Data]
  items                    :: a -> [Item.Data]

data Data = MkData {
  _header1                  :: Header1.Data,
  _original_network_id      :: Word16,
  _header2                  :: Header2.Data,
  _reserved_future_use      :: Word8,
  _broadcast_view_propriety :: Bool,
  _first_descriptors_length :: BytesLen,
  _descriptors              :: Vector Descriptor.Data,
  _items                    :: Vector Item.Data,
  _footer                   :: Footer.Data
  } deriving (Show)

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance Footer.Class Data where
  footer = _footer
  
instance Class Data where
  original_network_id      = _original_network_id  
  reserved_future_use      = _reserved_future_use
  broadcast_view_propriety = _broadcast_view_propriety 
  first_descriptors_length = _first_descriptors_length 
  descriptors              = toList . _descriptors
  items                    = toList . _items

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0024]
  table_ids _ = [0xC4]

instance EmptyExist.Class Data where
  mkEmpty =  MkData {
    _header1                  = mkEmpty,
    _original_network_id      = mkEmpty,
    _header2                  = mkEmpty,
    _reserved_future_use      = mkEmpty,
    _broadcast_view_propriety = mkEmpty,
    _first_descriptors_length = mkEmpty,
    _descriptors              = Data.Vector.empty,
    _items                    = Data.Vector.empty,
    _footer                   = mkEmpty
  }

instance Base.Class Data where
  parseAfterHeader1 h bs =
    let (footer,bs0)               = fromByteStringWithRest bs
        ((original_network_id,
           header2,
           w16),bs1)               = fromByteStringWithRest bs0
        reserved_future_use          = toWord8 $ (`shiftR` 13) $ w16 .&. 0xE000
        broadcast_view_propriety     = (/= 0)  $ (`shiftR` 12) $ w16 .&. 0x1000
        first_descriptors_length     = w16 .&. 0x0FFF
        (bs3,bs4)                    = BS.splitAt (fromInteger $ toInteger first_descriptors_length) bs1
        descriptors                  = fromByteString bs3
        items                        = fromByteString bs4
        d = MkData {
          _header1                      = h,
          _original_network_id          = original_network_id,
          _header2                      = header2,
          _reserved_future_use          = reserved_future_use,
          _broadcast_view_propriety     = broadcast_view_propriety,
          _first_descriptors_length     = first_descriptors_length,
          _descriptors                  = descriptors,
          _items                        = items,
          _footer                       = footer
          }
    in Result.Parsed d
