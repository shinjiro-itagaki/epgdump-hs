module Descriptor.PartialReception (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header

class Base.Class a => Class a where
  service_ids :: a -> [Word16]

data Data = MkData {
  _header      :: Header.Data, 
  _service_ids :: [Word16]
  } deriving (Show)

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  service_ids = _service_ids
