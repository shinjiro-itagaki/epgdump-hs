module Descriptor.CarouselCompatibleComposite.Title where
import Descriptor.Common(LangCode)
import Descriptor.CarouselCompatibleComposite.Common(SubDescriptor(..))
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (SubDescriptor a,HasText a, HasISO_639_LanguageCode a) => Title a where

data TitleData = MkTitleData {
  _title_descriptor_tag    :: Word8,
  _title_descriptor_length :: Word8,
  _title_text              :: String,
  _title_iso_639_language_code :: LangCode
  }
  
instance Descriptor TitleData where
  descriptor_tag = _title_descriptor_tag
  descriptor_length = _title_descriptor_length
  
instance HasText TitleData where
  text = _title_text
  
instance HasISO_639_LanguageCode TitleData where
  iso_639_language_code = _title_iso_639_language_code
