module Descriptor.DataContents where
import Descriptor.Common(Base,HasISO_639_LanguageCode, HasTextAndLen, HasSelector, HasDataComponentID)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasISO_639_LanguageCode a, HasTextAndLen a, HasSelector a, HasDataComponentID a) => DataContents a where
--  data_component_id :: a -> Word16
  entry_component   :: a -> Word8
--  selector_length   :: a -> Word8
--  selector_bytes    :: a -> [Word8]
  num_of_component_ref :: a -> Word8
  component_ref :: a -> [Word8]
--  iso_639_language_code :: a -> LangCode
--  text_length :: a -> Word8
--  text :: a -> String
