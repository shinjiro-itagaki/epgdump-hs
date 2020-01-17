module SITables.RST(
  Data,
  Class(..)
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(SITableIDs(..))
import qualified SITables.Base as Base
import Common(ByteString,HasOriginalNetworkID(..),EmptyExist(..),PID,TableID,BytesHolderIO(..),TableID,PID,PIDs(..))
import qualified SITables.Items as Items
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
--import Descriptor(HasServiceID(..),HasEventID(..))
import qualified Descriptor
import Parser(HasParser(..),FromWord64(..),ParseResult(..),ParseIOFlow(..),(>>==))
import qualified SITables.Base as Base
import Data.Vector(Vector,toList,empty,snoc)
import qualified SITables.RST.Item as Item

class (Header1.Class a) => Class a where
  items :: a -> [Item.Data]

data Data = MkData {
  _header1 :: Header1.Data,
  _items   :: Vector Item.Data
  }

instance SITableIDs Data where
  pids      _ =  MkExcludePIDs [0x0013]
  table_ids _ = [0x71]

instance Header1.Class Data where
  header1 = _header1
  setHeader1 x h = x {_header1 = h}    

instance Class Data where
  items = toList . _items

_parseIOFlow :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow bh init = Items.gather addItem' (Base.section_length_without_crc init) bh init
  where
    addItem' :: Data -> Item.Data -> Data
    addItem' x item = x {_items = (snoc (_items x) item)  }


instance Base.Class Data where
  footer  _ = Nothing
  parseIOFlowAfterHeader1 =
    flowStart
    |>>= _parseIOFlow    

instance HasParser Data where

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty Data.Vector.empty
