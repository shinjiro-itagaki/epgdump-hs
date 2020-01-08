module Descriptor.CarouselCompatibleComposite.Name where
import Descriptor.CarouselCompatibleComposite.Common(SubDescriptor(..))
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

  
class (SubDescriptor a, HasName a) => Name a where

data NameData = MkNameData {
  _name_descriptor_tag    :: Word8,
  _name_descriptor_length :: Word8,
  _name_name              :: String
  }
  
instance HasName NameData where
  name = _name_name

instance Descriptor NameData where
  descriptor_tag = _name_descriptor_tag
  descriptor_length = _name_descriptor_length
