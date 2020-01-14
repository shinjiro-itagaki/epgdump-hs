module SITables.RST(
  Data,
  Class(..),
  Item,
  pids, table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(HasOriginalNetworkID(..),PIDs(..),TableID)
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Descriptor(HasServiceID(..),HasEventID(..))
import qualified Descriptor

-- pids, table_ids
pids :: [Word64]
pids = [0x0013]

table_ids :: [TableID]
table_ids = [0x71]

class (Header1.Class a) => Class a where
  sections :: a -> [Item]

data Data = MkData {
  _header1 :: Header1.Data,
  _sections :: [Item]
  }

instance Header1.Class Data where
  header1 = _header1

data Item = MkItem {
  transport_stream_id  :: Word16,
  _original_network_id :: Word16,
  _service_id          :: Word16,
  _event_id            :: Word16,
  running_status       :: Word16
  }

instance HasOriginalNetworkID Item where
  original_network_id = _original_network_id

instance HasServiceID Item where
  service_id = service_id

instance HasEventID Item where
  event_id = event_id  

instance Class Data where
  sections = _sections
