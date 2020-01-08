module Descriptor.AudioComponent where
import Descriptor.Common(Base,HasComponent,HasISO_639_LanguageCode,HasText,LangCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasComponent a, HasISO_639_LanguageCode a, HasText a) => AudioComponent a where
-- reserved_future_use :: a -> Word8
--  stream_content :: a -> Word8
--  component_type :: a -> Word8
--  component_tag  :: a -> Word8
  stream_type    :: a -> Word8
  simulcast_group_tag :: a -> Word8
  es_multi_lingual_flag :: a -> Bool
  main_component_flag :: a -> Bool
  quality_indicator :: a -> Word8
  sampling_rate :: a -> Word8
-- reserved_future_use :: a -> Word8
--  iso_639_language_code :: a -> LangCode
  iso_639_language_code2 :: a -> Maybe LangCode
--  text :: a -> String


