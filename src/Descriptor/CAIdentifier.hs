module Descriptor.CAIdentifier (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import qualified Descriptor.LangCode as LangCode
import Data.Vector(Vector,empty,toList,snoc)

class Base.Class a => Class a where
  ca_system_id :: a -> Word16
  
data Data = MkData {
  _ca_system_id      :: Word16
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where  
  ca_system_id = _ca_system_id

