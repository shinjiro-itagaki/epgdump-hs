module Descriptor.CarouselCompatibleComposite.SubDescriptor.Type where
import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base
import Descriptor.Common(HasText(..),HasName(..),HasText(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

import Common(ByteString)
import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base

class (Base.Class a, HasText a) => Class a where

data Data = MkData {
  _header :: Header.Data,
  _text   :: String
  }

instance HasText Data where
  text = _text

