module Descriptor.ServiceGroup (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

class (Base.Class a) => Class a where
  service_group_type  :: a -> ServiceGroupType
  reserved_future_use :: a -> Word8
  service_group_data  :: a -> [ServiceGroupData]
  
data ServiceGroupType = Simultaneous Word8 | Undefined Word8 deriving (Show)

data ServiceGroupData = ServiceID {
  primary_service_id :: Word16,
  secondary_service_id :: Word16
  } | PrivateData ByteString
  deriving (Show)

data Data = MkData {
  _header              :: Header.Data,
  _service_group_type  :: ServiceGroupType,
  _reserved_future_use :: Word8,
  _service_group_data  :: Vector ServiceGroupData
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  service_group_type  = _service_group_type
  reserved_future_use = _reserved_future_use
  service_group_data  = toList . _service_group_data
