module SITables.TOT (
  Data,
  Class(..),
  pids, table_ids    
  ) where
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import qualified SITables.TDT
import qualified SITables.Base as Base
import qualified Parser.Result as Result
import Utils
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs
import qualified Utils.TimeDate as TimeDate
import Data.Vector(toList)

class (SITables.TDT.Class a) => Class a where
  reserved                :: a -> Word8
  descriptors_loop_length :: a -> Word16
  descriptors             :: a -> [Descriptor.Data]

data Data = MkData {
  _header1 :: Header1.Data,
  _jst_time :: TimeDate.Data,
  _reserved :: Word8,
  _descriptors_loop_length :: Word16,
  _descriptors :: Vector Descriptor.Data,
  _footer :: Footer.Data
  } deriving (Show)

instance Class Data where
  reserved                = _reserved
  descriptors_loop_length = _descriptors_loop_length
  descriptors             = toList . _descriptors
  
instance Header1.Class Data where
  header1 = _header1
  
instance SITables.TDT.Class Data where
  jst_time  = _jst_time

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty

instance Base.Class Data where
  footer = Just . _footer
  parseAfterHeader1 h bs =
    let ((jst_time,w16),bs0)    = fromByteStringWithRest bs
        reserved                = fromWord16 $ (w16 .&. 0xF000) `shiftR` 12
        descriptors_loop_length =               w16 .&. 0x0FFF
        (descriptors,bs1)       = fromByteStringWithRest bs0
        footer                  = fromByteString bs1
        d = MkData {
          _header1  = h,
          _jst_time = jst_time,
          _reserved = reserved,
          _descriptors_loop_length = descriptors_loop_length,
          _descriptors = descriptors,
          _footer = footer
          }
    in Result.Parsed d

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0014]
  table_ids _ = [0x73]

