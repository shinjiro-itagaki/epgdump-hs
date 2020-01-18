module Descriptor.Stuffing (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header

class Base.Class a => Class a where
  stuffing_bytes :: a -> [Word8]

data Data = MkData {
  _header            :: Header.Data,
  _stuffing_bytes    :: [Word8]
  }
  
instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  stuffing_bytes = _stuffing_bytes
