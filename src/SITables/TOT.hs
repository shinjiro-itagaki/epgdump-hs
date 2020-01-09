module SITables.TOT where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(CommonHeader(..) ,CommonHeader2(..),HasDescriptors(..))
import Common(HasOriginalNetworkID(..))
import Descriptor(HasServiceID(..))
import qualified Descriptor
import qualified SITables.TDT
                 

class (SITables.TDT.Class a, HasDescriptors a) => Class a where
-- reserved
  descriptor_loop_length :: a -> Word16

data Data = MkData {
  -- CommonHeader 
  _table_id                    :: Word8, -- h->table_id = getBit(data, &boff, 8);
  _section_syntax_indicator    :: Bool, -- h->section_syntax_indicator = getBit(data, &boff, 1);
  _reserved_future_use         :: Bool, -- h->reserved_future_use = getBit(data, &boff, 1);
  _reserved1                   :: Word8, -- h->reserved1 = getBit(data, &boff, 2);
  _section_length              :: Word16, -- h->section_length =getBit(data, &boff,12);

  _jst_time :: Word64,
  _reserved :: Word8,
  _descriptor_loop_length :: Word16,
  _descriptors :: [Descriptor.Data]
  }

instance CommonHeader Data where
  table_id                 = _table_id
  section_syntax_indicator = _section_syntax_indicator
  reserved_future_use      = _reserved_future_use
  reserved1                = _reserved1
  section_length           = _section_length
  
instance SITables.TDT.Class Data where
  jst_time  = _jst_time 

instance HasDescriptors Data where
  descriptors = _descriptors
