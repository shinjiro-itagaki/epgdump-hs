module SITables.BAT (
  Data,
  Class(..),
  Item,
  pids,
  table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
--import Common(HasOriginalNetworkID(..),HasParser(..),ParseResult(..))
import Common(HasOriginalNetworkID(..))
import qualified Descriptor

pids :: [Word64]
pids = [0x0011]

table_ids :: [Word32]
table_ids = [0x4A]

class (Header1.Class a, Header2.Class a, HasDescriptors a) => Class a where
  bouquet_id                   :: a -> Word16
  bouquet_descriptors_length   :: a -> Word16
  transport_stream_loop_length :: a -> Word16
  transport_streams            :: a -> [Item]

data Data = MkData {
  
  _header1    :: Header1.Data,
  
  _bouquet_id                   :: Word16,
  _bouquet_descriptors_length   :: Word16, 
  _descriptors                  :: [Descriptor.Data],
  _transport_stream_loop_length :: Word16,
  _transport_streams            :: [Item],
  
  -- CommonHeader2
  _header2    :: Header2.Data 
  }

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2
  
instance HasDescriptors Data where
  descriptors = _descriptors

instance Class Data where
  bouquet_id                   = _bouquet_id
  bouquet_descriptors_length   = _bouquet_descriptors_length
  transport_stream_loop_length = _transport_stream_loop_length
  transport_streams            = _transport_streams
  

data Item = MkItem {
  transport_stream_id          :: Word16,
  _original_network_id         :: Word16,
  transport_descriptors_length :: Word16,
  __descriptors                :: [Descriptor.Data]
  }

instance HasOriginalNetworkID Item where
  original_network_id = _original_network_id

instance HasDescriptors Item where
  descriptors = __descriptors

--instance HasParser Data where
--  parse bytes = NotMatch
