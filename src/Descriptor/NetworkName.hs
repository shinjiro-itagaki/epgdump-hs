module Descriptor.NetworkName (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import Descriptor.Common(HasName(..))
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

class (Base.Class a, HasName a) => Class a where

data Data = MkData {
  _name :: String
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance HasName Data where
  name = _name

instance Class Data where
