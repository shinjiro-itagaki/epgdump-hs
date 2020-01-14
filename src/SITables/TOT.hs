module SITables.TOT (
  Data,
  Class(..),
  pids, table_ids    
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Common(HasOriginalNetworkID(..))
import Descriptor(HasServiceID(..))
import qualified Descriptor
import qualified SITables.TDT

pids :: [Word64]
pids = [0x0014]

table_ids :: [Word32]
table_ids = [0x73]

class (SITables.TDT.Class a, HasDescriptors a) => Class a where
-- reserved
  descriptor_loop_length :: a -> Word16

data Data = MkData {
  _header1 :: Header1.Data,
  _jst_time :: Word64,
  _reserved :: Word8,
  _descriptor_loop_length :: Word16,
  _descriptors :: [Descriptor.Data]
  }

instance Header1.Class Data where
  header1 = _header1
  
instance SITables.TDT.Class Data where
  jst_time  = _jst_time 

instance HasDescriptors Data where
  descriptors = _descriptors
