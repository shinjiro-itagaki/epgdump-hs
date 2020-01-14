module SITables.SDT(
  Data,
  Class(..),
  pids, table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Common(HasOriginalNetworkID(..),HasServiceID(..))
import qualified Descriptor

-- pids, table_ids
pids :: [Word64]
pids = [0x0011]

table_ids :: [Word32]
table_ids = [0x42,0x46]

class (Header1.Class a, Header2.Class a, HasOriginalNetworkID a) => Class a where
  transport_stream_id :: a -> Word16

data Data = MkData {
  _header1             :: Header1.Data,
  _transport_stream_id :: Word16,
  _header2             :: Header2.Data,
  _original_network_id :: Word16  
  }

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2
  
instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance Class Data where
  transport_stream_id = _transport_stream_id

class (Common.HasServiceID a, HasDescriptors a) => ItemClass a where
  

data Item = MkItem {
  _service_id  :: Word16,
  _eit_user_defined_flags :: Word8,
  _eit_schedule_flag :: Bool,
  _eit_present_following_flag :: Bool,
  _running_status :: Word8,
  _free_CA_mode :: Bool,
  _descriptors_loop_length :: Word16,
  _descriptors                :: [Descriptor.Data]
  }

instance HasDescriptors Item where
  descriptors = _descriptors

instance HasServiceID Item where
  service_id = _service_id
