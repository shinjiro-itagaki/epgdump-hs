module SITables.ST(
  Data,
  Class(..),
  ) where
import qualified SITables.Base as Base
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified Descriptor
import qualified Parser.Result as Result
import Utils
import qualified SITables.Base as Base
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs
import qualified Utils.FromByteString as FromByteString

class (Base.Class a) => Class a where
  data_bytes :: a -> ByteString

data Data = MkData {
  _header1    :: Header1.Data,
  _data_bytes :: ByteString
  } deriving (Show)

instance SITableIDs.Class Data where
  pids      _ =  MkExcludePIDs [0x0000,0x0001,0x0014]
  table_ids _ = [0x72]

instance Base.Class Data where
  footer  _ = Nothing

instance Class Data where
  data_bytes = _data_bytes

instance Header1.Class Data where
  header1 = _header1

instance EmptyExist.Class Data where
  mkEmpty = MkData {
  _header1    = mkEmpty,
  _data_bytes = mkEmpty
  }
