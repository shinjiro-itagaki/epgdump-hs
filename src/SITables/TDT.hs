module SITables.TDT (
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

pids :: [Word64]
pids = [0x0014]

table_ids :: [Word32]
table_ids = [0x70]

class (Header1.Class a) => Class a where
  jst_time :: a -> Word64

data Data = MkData {
  -- CommonHeader 
  _header1 :: Header1.Data,
  _jst_time :: Word64
  }

instance Header1.Class Data where
  header1 = _header1
  
instance Class Data where
  jst_time  = _jst_time 
