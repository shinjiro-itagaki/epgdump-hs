module Descriptor.EventGroup (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.EventInfo as EventInfo
import qualified Descriptor.Link.EventInfo as LinkEventInfo

class (Base.Class a) => Class a where
  group_type  :: a -> GroupType
  event_count :: a -> Word8
  events      :: a -> [EventInfo.Data]
  groups      :: a -> [GroupInfo]

data Data = MkData {
  _group_type  :: Word8,
  _event_count :: Word8,
  _events      :: [EventInfo.Data],
  _groups      :: Vector GroupInfo
  } deriving (Show)

data GroupType =
  Undefined --0x0 or 0x6 - 0xF,
  | Common -- 0x1
  | Relay -- -0x2
  | Movement -- 0x3
  | RelayToOtherNetworks -- 0x4
  | MovementFromOtherNetworks -- 0x5
  deriving (Show)

fromNum :: (Num a,Eq a) => a -> GroupType
fromNum i
  | i == 0x1 = Common
  | i == 0x2 = Relay
  | i == 0x3 = Movement
  | i == 0x4 = RelayToOtherNetworks
  | i == 0x5 = MovementFromOtherNetworks
  | otherwise = Undefined

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  group_type  = fromNum . _group_type
  event_count = _event_count
  events      = _events
  groups      = toList . _groups

data GroupInfo =
  Others       ByteString
  | RelayTo      LinkEventInfo.Data
  | MovementFrom LinkEventInfo.Data
  deriving (Show)
