module Descriptor.AudioComponent (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasComponentTag(..),HasComponent(..),HasISO_639_LanguageCode(..),HasText(..),LangCode,Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasComponentTag a, HasComponent a, HasISO_639_LanguageCode a, HasText a) => Class a where
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

data Data = MkData {
  _descriptor_tag         :: Word8,
  _descriptor_length      :: Word8,
  _stream_content         :: Word8,
  _component_type         :: Word8,
  _component_tag          :: Word8,
  _stream_type            :: Word8,
  _simulcast_group_tag    :: Word8,
  _es_multi_lingual_flag  :: Bool,
  _main_component_flag    :: Bool,
  _quality_indicator      :: Word8,
  _sampling_rate          :: Word8,
  _iso_639_language_code  :: LangCode,
  _iso_639_language_code2 :: Maybe LangCode,
  _text                   :: String  
}

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)
  
instance HasText Data where
  text = _text

instance HasComponent Data where
  component_type = _component_type
  stream_content = _stream_content

instance HasComponentTag Data where
  component_tag = _component_tag
  
instance HasISO_639_LanguageCode Data where
  iso_639_language_code = _iso_639_language_code

instance Class Data where
  stream_type            = _stream_type
  simulcast_group_tag    = _simulcast_group_tag
  es_multi_lingual_flag  = _es_multi_lingual_flag
  main_component_flag    = _main_component_flag
  quality_indicator      = _quality_indicator
  sampling_rate          = _sampling_rate
  iso_639_language_code2 = _iso_639_language_code2
