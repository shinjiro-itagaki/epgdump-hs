module Descriptor.NVOD_Reference (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import Descriptor.Common(HasName(..))
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.Link.ServiceInfo as ServiceInfo

class Base.Class a => Class a where
  references :: a -> [ServiceInfo.Data]

data Data = MkData {
  _references :: Vector ServiceInfo.Data
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  references = toList . _references
