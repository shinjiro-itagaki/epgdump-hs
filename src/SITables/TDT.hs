module SITables.TDT (
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
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs

import Utils

class (Header1.Class a) => Class a where
  jst_time :: a -> Word64

data Data = MkData {
  -- CommonHeader 
  _header1 :: Header1.Data,
  _jst_time :: Word64
  } deriving (Show)

instance Header1.Class Data where
  header1 = _header1
  
instance Class Data where
  jst_time = _jst_time 

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty


-- _parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow fh init = do
--   getBitsIO_M fh [
--     (40, (\(v,d) -> d { _jst_time = fromWord64 v}))
--     ] init
  
instance Base.Class Data where
  footer _ = Nothing
  -- parseIOFlowAfterHeader1 =
  --   flowStart
  --   |>>= _parseIOFlow
 
instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0014]
  table_ids _ = [0x70]

instance FromByteString.Class Data where
