module Descriptor.EventInfo (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

class (Show a) => Class a where
  service_id :: a -> Word16
  event_id   :: a -> Word16

data Data = MkData {
  _service_id :: Word16,
  _event_id   :: Word16
  } deriving (Show,Eq)

instance Class Data where
  service_id = _service_id
  event_id   = _event_id
