module Descriptor.ShortEvent (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasISO_639_LanguageCode(..),HasTextAndLen(..),LangCode,Descriptor(..),HasTextAndLen(..), HasText(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasISO_639_LanguageCode a, HasTextAndLen a) => Class a where
-- iso_639_language_code :: a -> LangCode
  event_name_length :: a -> Word8
  event_name        :: a -> String
--  text_length :: a -> Word8
--  text :: a -> String

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _event_name_length :: Word8,
  _event_name        :: String,
  _text_length       :: Word8,
  _text              :: String,
  _iso_639_language_code :: LangCode
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)
  
instance HasISO_639_LanguageCode Data where
  iso_639_language_code = _iso_639_language_code

instance HasTextAndLen Data where
  text_length = _text_length

instance HasText Data where
  text = _text

instance Class Data where
  event_name_length = _event_name_length
  event_name        = _event_name
