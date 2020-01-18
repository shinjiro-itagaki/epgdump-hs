module Descriptor.StreamIdentifier (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(HasComponentTag(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header

class (Base.Class a, HasComponentTag a) => Class a where
--  component_tag :: a -> Word8
  
data Data = MkData {
  _header        :: Header.Data,
  _component_tag :: Word8
  }
  
instance Base.Class Data where

instance HasComponentTag Data where
  component_tag = _component_tag
  
instance Class Data where
