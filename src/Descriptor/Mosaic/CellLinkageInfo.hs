module Descriptor.Mosaic.CellLinkageInfo where
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Descriptor.Link.EventInfo as EventInfo

import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)

data Data =
  Bouquet Word16 -- 0x01
  | Service ServiceInfo.Data -- 0x02,0x03 
  | Event EventInfo.Data -- 0x04
  deriving (Show)
