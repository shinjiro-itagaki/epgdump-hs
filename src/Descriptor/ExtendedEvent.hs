module Descriptor.ExtendedEvent where
import Descriptor.Common(Base,HasISO_639_LanguageCode,HasTextAndLen)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasISO_639_LanguageCode a, HasTextAndLen a) => ExtendedEvent a where
  descriptor_number :: a -> Word8
  last_descriptor_number :: a -> Word8
--  iso_639_language_code :: a -> LangCode
  length_of_items :: a -> Word8
  extended_event_items :: a -> [ExtendedEventItem]
--  text_length :: a -> Word8
--  text :: a -> String

data ExtendedEventItem = MkExtendedEventItem {
  item_description_length :: Word8,
  item_description_chars :: String,
  item_length :: Word8,
  item_chars :: String
  }
