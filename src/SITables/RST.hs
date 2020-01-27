module SITables.RST(
  Data,
  Class(..)
  ) where

import qualified SITables.Base as Base
import qualified BytesReader.Base as BytesReaderBase
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified Descriptor
import qualified Parser.Result as Result
import qualified SITables.Base as Base
import Data.Vector(Vector,toList,empty,snoc)
import qualified SITables.RST.Item as Item
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs
import Utils

class (Header1.Class a) => Class a where
  items :: a -> [Item.Data]

data Data = MkData {
  _header1 :: Header1.Data,
  _items   :: Vector Item.Data
  } deriving (Show)

instance SITableIDs.Class Data where
  pids      _ =  MkExcludePIDs [0x0013]
  table_ids _ = [0x71]

instance Header1.Class Data where
  header1 = _header1

instance Class Data where
  items = toList . _items

-- _parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow bh init = Items.gather addItem' (Base.section_length_without_crc init) bh init
--   where
--     addItem' :: Data -> Item.Data -> Data
--     addItem' x item = x {_items = (snoc (_items x) item)  }


instance Base.Class Data where
  footer  _ = Nothing
  -- parseIOFlowAfterHeader1 =
  --   flowStart
  --   |>>= _parseIOFlow    

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty Data.Vector.empty

instance FromByteString.Class Data where
