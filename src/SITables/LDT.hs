module SITables.LDT (
  Data,
  Class(..),
  Item,
  pids,
  table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..),Schedule(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Common(HasOriginalNetworkID(..))
import Descriptor(HasServiceID(..),HasEventID(..))
import qualified Descriptor

pids :: [Word64]
pids = [0x0025]

table_ids :: [Word32]
table_ids = [0xC7]

class (Header1.Class a, Header2.Class a, HasOriginalNetworkID a) => Class a where
  original_service_id :: a -> Word16
  transport_stream_id :: a -> Word16
  
  
data Data = MkData {
  _header1 :: Header1.Data,
  _original_service_id :: Word16,
  _header2 :: Header2.Data,
  _transport_stream_id    :: Word16,
  _original_network_id    :: Word16,
  _items :: [Item]
  }

class (HasDescriptors a) => ItemClass a where
  description_id          :: a -> Word16
  descriptors_loop_length :: a -> Word16

data Item = MkItem {
  _description_id :: Word16,
-- reserved_future_use,  
  _descriptors_loop_length :: Word16,
  _descriptors :: [Descriptor.Data]
  }

instance HasDescriptors Item where
  descriptors = _descriptors

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance Class Data where
  original_service_id = _original_service_id
  transport_stream_id = _transport_stream_id

instance ItemClass Item where
  description_id = _description_id 
  descriptors_loop_length = _descriptors_loop_length
