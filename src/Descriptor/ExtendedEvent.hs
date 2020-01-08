module Descriptor.ExtendedEvent (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasISO_639_LanguageCode(..),HasText(..),HasTextAndLen(..),Descriptor(..),LangCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasISO_639_LanguageCode a, HasTextAndLen a) => Class a where
  descriptor_number :: a -> Word8
  last_descriptor_number :: a -> Word8
--  iso_639_language_code :: a -> LangCode
  length_of_items :: a -> Word8
  extended_event_items :: a -> [Item]
--  text_length :: a -> Word8
--  text :: a -> String

data Data = MkData {
  _descriptor_tag         :: Word8,
  _descriptor_length      :: Word8,
  _descriptor_number      :: Word8,
  _last_descriptor_number :: Word8,
  _iso_639_language_code  :: LangCode,
  _length_of_items        :: Word8,
  _extended_event_items   :: [Item],
  _text_length            :: Word8,
  _text                   :: String  
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
  
instance HasTextAndLen Data where
  text_length = _text_length

instance Class Data where
  descriptor_number      = _descriptor_number
  last_descriptor_number = _last_descriptor_number
  length_of_items        = _length_of_items
  extended_event_items   = _extended_event_items
  
data Item = MkItem {
  item_description_length :: Word8,
  item_description_chars :: String,
  item_length :: Word8,
  item_chars :: String
  }
