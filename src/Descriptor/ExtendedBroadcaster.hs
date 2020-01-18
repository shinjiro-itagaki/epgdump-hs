module Descriptor.ExtendedBroadcaster (
  Class(..)
  ,Data
  ) where
import Common(HasOriginalNetworkID(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.ExtendedEvent.Broadcaster as Broadcaster

class (Base.Class a) => Class a where
  broadcaster_type    :: a -> Word8 -- 4
  reserved_future_use :: a -> Word8 -- 4
  broadcaster         :: a -> Broadcaster.Data

data Data = MkData {
  _header           :: Header.Data,
  _broadcaster_type :: Word8,
  _broadcaster      :: Broadcaster.Data
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where  
  broadcaster_type  = _broadcaster_type
  broadcaster       = _broadcaster
