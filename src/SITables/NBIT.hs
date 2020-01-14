module SITables.NBIT (
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
table_ids = [0xC5,0xC6]

class (Header1.Class a, Header2.Class a, HasOriginalNetworkID a) => Class a where
  
data Data = MkData {
  _header1             :: Header1.Data,
  _original_network_id :: Word16,  
  _header2             :: Header2.Data,
  _items               :: [Item]
  }

class (HasDescriptors a) => ItemClass a where
  information_id            :: a -> Word16
  information_type          :: a -> Word8
  description_body_location :: a -> (Bool,Bool)
  user_defined              :: a -> Word8
  number_of_keys            :: a -> Word8
  keys                      :: a -> [Word16]
  descriptors_loop_length   :: a -> Word16
  
data Item = MkItem {
  _information_id :: Word16,
  _information_type :: Word8,
  _description_body_location :: (Bool,Bool),
-- reserved_future_use,
  _user_defined :: Word8,
  _number_of_keys :: Word8,
  _keys :: [Word16],
-- reserved_future_use
  _descriptors_loop_length :: Word16,
  _descriptors :: [Descriptor.Data]
  }

instance HasDescriptors Item where
  descriptors = _descriptors

instance ItemClass Item where
  information_id = _information_id
  information_type = _information_type 
  description_body_location = _description_body_location
  user_defined = _user_defined
  number_of_keys = _number_of_keys
  keys = _keys
  descriptors_loop_length = _descriptors_loop_length

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance Class Data where
