module Descriptor.CarouselCompatibleComposite.Type where
import Descriptor.CarouselCompatibleComposite.Common(SubDescriptor(..))
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (SubDescriptor a, HasText a) => Class a where

data Data = MkData {
  _type_descriptor_tag    :: Word8,
  _type_descriptor_length :: Word8,
  _type_text              :: String
  }

instance Descriptor Data where
  descriptor_tag = _type_descriptor_tag
  descriptor_length = _type_descriptor_length

instance HasText Data where
  text = _type_text

