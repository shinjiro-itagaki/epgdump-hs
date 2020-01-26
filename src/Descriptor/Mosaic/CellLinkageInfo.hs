-- 6.2.9
module Descriptor.Mosaic.CellLinkageInfo where
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Descriptor.Link.EventInfo as EventInfo

import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import qualified Utils.FromByteString as FromByteString
import Utils.FromByteString(fromByteStringWithRest)
import qualified Data.ByteString.Lazy as BS

class (FromByteString.Class a) => Class a where
  cell_linkage      :: a -> Data
  
  cell_linkage_info :: a -> Word8
  cell_linkage_info = cell_linkage_info . cell_linkage
  
  info :: a -> Info
  info = info . cell_linkage

data Data = MkData {
  _cell_linkage_info :: Word8,
  _info              :: Info
  } deriving (Show,Eq)

instance Class Data where
  cell_linkage x = x
  cell_linkage_info = _cell_linkage_info
  info              = _info

data Info = 
  Bouquet Word16 -- 0x01
  | Service ServiceInfo.Data -- 0x02,0x03 
  | Event EventInfo.Data -- 0x04
  | None -- otherwise
  deriving (Show,Eq)

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    case BS.uncons bs of
      Just (y,bs0) -> FromByteString.map (\info -> MkData y info) $ case y of
        0x01 -> FromByteString.map Bouquet $ fromByteStringWithRest bs0
        0x02 -> FromByteString.map Service $ fromByteStringWithRest bs0
        0x03 -> FromByteString.map Service $ fromByteStringWithRest bs0
        0x04 -> FromByteString.map Event   $ fromByteStringWithRest bs0
        _    -> (None,bs0)
      Nothing -> (MkData 0 None,bs)
