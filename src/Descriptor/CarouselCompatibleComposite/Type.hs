module Descriptor.CarouselCompatibleComposite.Type where
import Descriptor.CarouselCompatibleComposite.Common(SubDescriptor(..))
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (SubDescriptor a, HasText a) => Type a where

data TypeData = MkTypeData {
  _type_descriptor_tag    :: Word8,
  _type_descriptor_length :: Word8,
  _type_text              :: String
  }

instance Descriptor TypeData where
  descriptor_tag = _type_descriptor_tag
  descriptor_length = _type_descriptor_length

instance HasText TypeData where
  text = _type_text

