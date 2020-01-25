module Descriptor.NVOD_Reference (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.Link.ServiceInfo as ServiceInfo

class Base.Class a => Class a where
  references :: a -> [ServiceInfo.Data]

data Data = MkData {
  _header     :: Header.Data, 
  _references :: Vector ServiceInfo.Data
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  references = toList . _references
