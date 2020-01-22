module Descriptor.CarouselCompatibleComposite.SubDescriptor.Type where
import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

import Common(ByteString)
import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base

class (Base.Class a) => Class a where
  text   :: a -> String
  
data Data = MkData {
  _header :: Header.Data,
  _text   :: String
  } deriving (Show)

instance Header.Class Data where

instance Base.Class Data where

instance Class Data where
  text = _text

