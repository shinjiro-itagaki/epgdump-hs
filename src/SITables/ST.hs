module SITables.ST(
  Data(data_bytes),
  Class(..),
  pids, table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common
import Common(HasOriginalNetworkID(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Descriptor(HasServiceID(..),HasEventID(..))
import qualified Descriptor

data PIDS = Exclude [Word64]

instance MatchPID PIDS where
  -- 仕様書にはExclude 0x0000,0x0001,0x0014 と書かれており、３つ以外の値という意味で合っているのかよくわからないため、とりあえずFalseを返すようにしておく
  match_pid (Exclude (x:[])) y = False
--  match_pid (Exclude (x:xs)) y = if x == y then True else match_pid xs y
-- pids, table_ids

pids :: PIDS
pids = Exclude [0x0000,0x0001,0x0014]

table_ids :: [Word32]
table_ids = [0x72]

class (Header1.Class a) => Class a where

data Data = MkData {
  -- CommonHeader 
  _table_id                    :: Word8, -- h->table_id = getBit(data, &boff, 8);
  _section_syntax_indicator    :: Bool, -- h->section_syntax_indicator = getBit(data, &boff, 1);
  _reserved_future_use         :: Bool, -- h->reserved_future_use = getBit(data, &boff, 1);
  _reserved1                   :: Word8, -- h->reserved1 = getBit(data, &boff, 2);
  _section_length              :: Word16, -- h->section_length =getBit(data, &boff,12);
  data_bytes                   :: [Word8]
  }

instance Header1.Class Data where
  table_id                 = _table_id
  section_syntax_indicator = _section_syntax_indicator
  reserved_future_use      = _reserved_future_use
  reserved1                = _reserved1
  section_length           = _section_length
