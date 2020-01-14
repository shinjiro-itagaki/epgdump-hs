module SITables.BIT (
  Data,
  Class(..),
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
pids = [0x0024]

table_ids :: [Word32]
table_ids = [0xC4]

class (Header1.Class a ,Header2.Class a, HasOriginalNetworkID a) => Class a where
  broadcast_view_propriety :: a -> Bool
  first_descriptors_length :: a -> Word16
  first_descriptors        :: a -> [Descriptor.Data]
  broadcaster_id           :: a -> Word8
  broadcaster_descriptor_length :: a -> Word16
  broadcaster_descriptors  :: a -> [Descriptor.Data]

data Data = MkData {
  _header1             :: Header1.Data,
  _original_network_id :: Word16,  
  _header2             :: Header2.Data,
  _broadcast_view_propriety      :: Bool,
  _first_descriptors_length      :: Word16,
  _first_descriptors             :: [Descriptor.Data],
  _broadcaster_id                :: Word8,
  _broadcaster_descriptor_length :: Word16,
  _broadcaster_descriptors       :: [Descriptor.Data]
  }

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance Class Data where
  broadcast_view_propriety = _broadcast_view_propriety 
  first_descriptors_length = _first_descriptors_length 
  first_descriptors        = _first_descriptors        
  broadcaster_id           = _broadcaster_id           
  broadcaster_descriptor_length = _broadcaster_descriptor_length
  broadcaster_descriptors  = _broadcaster_descriptors  
