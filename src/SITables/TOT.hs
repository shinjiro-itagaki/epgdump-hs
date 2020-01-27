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

class (SITables.TDT.Class a) => Class a where
-- reserved
  descriptor_loop_length :: a -> Word16

data Data = MkData {
  _header1 :: Header1.Data,
  _jst_time :: Word64,
  _reserved :: Word8,
  _descriptors_loop_length :: Word16,
  _descriptors :: [Descriptor.Data],
  _footer :: Footer.Data
  } deriving (Show)

instance Header1.Class Data where
  header1 = _header1
  
instance SITables.TDT.Class Data where
  jst_time  = _jst_time 

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty [] mkEmpty


-- _parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow fh init = do
--   getBitsIO_M fh [
--     (40, (\(v,d) -> d { _jst_time                = fromWord64 v})),
--     ( 4, (\(v,d) -> d { _reserved                = fromWord64 v})),
--     (12, (\(v,d) -> d { _descriptors_loop_length = fromWord64 v}))
--     ] init

instance Base.Class Data where
  footer = Just . _footer
  -- parseIOFlowAfterHeader1 =
  --   flowStart
  --   |>>= _parseIOFlow

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0014]
  table_ids _ = [0x73]


instance FromByteString.Class Data where
