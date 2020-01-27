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
import qualified Utils.TimeDate as TimeDate

import Utils

class (Header1.Class a) => Class a where
  jst_time :: a -> TimeDate.Data

data Data = MkData {
  _header1 :: Header1.Data,
  _jst_time :: TimeDate.Data -- 40
  } deriving (Show)

instance Header1.Class Data where
  header1 = _header1
  
instance Class Data where
  jst_time = _jst_time 

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty
 
instance Base.Class Data where
  footer _ = Nothing
  parseAfterHeader1 h bs =
    let jst_time = fromByteString bs
        d = MkData {
          _header1  = h,
          _jst_time = jst_time
          }
    in Result.Parsed d
          
instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0014]
  table_ids _ = [0x70]
