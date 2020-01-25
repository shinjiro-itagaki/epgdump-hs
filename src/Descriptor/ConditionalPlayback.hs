
-- Specified in ARIB STD-B25
module Descriptor.ConditionalPlayback (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

class Base.Class a => Class a where
  
data Data = MkData {
  _header :: Header.Data
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
