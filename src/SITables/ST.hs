module SITables.ST(
  Data,
  Class(..),
  pids, table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common
import Common(HasOriginalNetworkID(..),PIDs(..),TableID)
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Descriptor(HasServiceID(..),HasEventID(..))
import qualified Descriptor

pids :: PIDs
pids = MkExcludePIDs [0x0000,0x0001,0x0014]

table_ids :: [TableID]
table_ids = [0x72]

class (Header1.Class a) => Class a where
  data_bytes :: a -> [Word8]

data Data = MkData {
  _header1    :: Header1.Data,
  _data_bytes :: [Word8]
  }

instance Class Data where
  data_bytes = _data_bytes

instance Header1.Class Data where
  header1 = _header1
