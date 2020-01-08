module Descriptor.ShortEvent where
import Descriptor.Common(Base,HasISO_639_LanguageCode,HasTextAndLen)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasISO_639_LanguageCode a, HasTextAndLen a) => ShortEvent a where
-- iso_639_language_code :: a -> LangCode
  event_name_length :: a -> Word8
  event_name :: a -> String
--  text_length :: a -> Word8
--  text :: a -> String

