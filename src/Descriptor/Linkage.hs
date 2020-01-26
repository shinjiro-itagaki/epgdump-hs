-- 6.2.8
module Descriptor.Linkage (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString,EmptyExist(..))
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import Utils.FromByteString(fromByteStringWithRest)
import qualified Data.ByteString.Lazy as BS


class (Base.Class a, ServiceInfo.Class a) => Class a where
  linkage_type        :: a -> LinkageType
  private_data_bytes  :: a -> ByteString

data Data = MkData {
  _header              :: Header.Data,
  _linkage_type        :: LinkageType, -- 8
  _service_info        :: ServiceInfo.Data,
  _private_data_bytes  :: ByteString -- [8]
  } deriving (Show)

data LinkageType = ReservedForFutureUse
                 | Information
                 | EPG
                 | CA_Replacement
                 | SI -- TS containing complete Network/Bouquet SI
                 | ServiceReplace
                 | DataBroadcast
                 | UserDefined
                 | ReservedForFutureUse2 -- Standardization organization defined area
                 | ReservedForReTransmission
                 deriving (Show,Eq)

instance EmptyExist LinkageType where
  mkEmpty = ReservedForFutureUse

toLinkageType :: Word8 -> LinkageType
toLinkageType x
  | 0x80 <=  x && x <= 0xBF = UserDefined
  | 0xC0 <=  x && x <= 0xFD = ReservedForFutureUse2
  | otherwise =
      case x of
        0x01 -> Information
        0x02 -> EPG
        0x03 -> CA_Replacement
        0x04 -> SI
        0x05 -> ServiceReplace
        0x06 -> DataBroadcast
        0xFE -> ReservedForReTransmission
        _    -> ReservedForFutureUse

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  fromByteStringAfterHeader h bs =
    let (bs0,rest) = BS.splitAt (fromInteger $ toInteger $ Header.descriptor_length h) bs
        (linkage_type,bs1) = fromByteStringWithRest bs0
        (service_info,bs2) = fromByteStringWithRest bs1
        private_data_bytes = bs2
        d = MkData h (toLinkageType linkage_type) service_info private_data_bytes
    in (Just d,rest)

instance ServiceInfo.Class Data where
  service_info = _service_info
  -- 通常のServiceInfoとはIDの配置順が異なるので関数を書き換えている
  transport_stream_id = ServiceInfo.id0 . _service_info  
  original_network_id = ServiceInfo.id1 . _service_info
  service_id          = ServiceInfo.id2 . _service_info  

instance Class Data where
  linkage_type       = _linkage_type
  private_data_bytes = _private_data_bytes
