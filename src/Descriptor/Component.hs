module Descriptor.Component (
    Class(..)
    ,Data
    ) where
import Descriptor.Common(Base(..),Descriptor(..),HasISO_639_LanguageCode(..),LangCode, HasText(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasISO_639_LanguageCode a, HasText a) => Class a where
  stream_content :: a -> Word8
  component_type :: a -> Word8
  component_tag  :: a -> Word8

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _stream_content    :: Word8,
  _component_type    :: Word8,
  _component_tag     :: Word8,
  _iso_639_language_code :: LangCode,
  _text              :: String
}

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance HasISO_639_LanguageCode Data where
  iso_639_language_code = _iso_639_language_code

instance HasText Data where
  text = _text
  
instance Class Data where
  stream_content = _stream_content
  component_type = _component_type
  component_tag  = _component_tag
