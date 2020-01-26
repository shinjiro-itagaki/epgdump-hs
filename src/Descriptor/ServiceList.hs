module Descriptor.ServiceList (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.ServiceList.Info as Info
import Data.Vector(Vector,empty,snoc,toList)
class Base.Class a => Class a where
  list :: a -> [Info.Data]

data Data = MkData {
  _header :: Header.Data,
  _list   :: Vector Info.Data
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  
instance Class Data where
  list = toList . _list
