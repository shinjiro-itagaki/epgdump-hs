
-- 6.2.26 Audio component descriptor
module Descriptor.AudioComponent (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import qualified Utils.LangCode as LangCode
import Data.Vector(Vector,empty,toList,snoc)

class (Base.Class a) => Class a where
  reserved_future_use1   :: a -> Word8
  stream_content         :: a -> Word8
  component_type         :: a -> Word8
  component_tag          :: a -> Word8
  stream_type            :: a -> Word8
  simulcast_group_tag    :: a -> Word8
  es_multi_lingual_flag  :: a -> Bool
  main_component_flag    :: a -> Bool
  quality_indicator      :: a -> Word8
  sampling_rate          :: a -> Word8
  reserved_future_use2   :: a -> Word8
  iso_639_language_code  :: a -> LangCode.Data
  iso_639_language_code2 :: a -> Maybe LangCode.Data
  text                   :: a -> String

data Data = MkData {
  _header                 :: Header.Data,
  _stream_content         :: Word8,
  _component_type         :: Word8,
  _component_tag          :: Word8,
  _stream_type            :: Word8,
  _simulcast_group_tag    :: Word8,
  _es_multi_lingual_flag  :: Bool,
  _main_component_flag    :: Bool,
  _quality_indicator      :: Word8,
  _sampling_rate          :: Word8,
  _iso_639_language_code  :: LangCode.Data,
  _iso_639_language_code2 :: Maybe LangCode.Data,
  _text                   :: String  
} deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)
  
instance Class Data where
  component_type         = _component_type
  stream_content         = _stream_content
  component_tag          = _component_tag
  stream_type            = _stream_type
  simulcast_group_tag    = _simulcast_group_tag
  es_multi_lingual_flag  = _es_multi_lingual_flag
  main_component_flag    = _main_component_flag
  quality_indicator      = _quality_indicator
  sampling_rate          = _sampling_rate
  iso_639_language_code  = _iso_639_language_code  
  iso_639_language_code2 = _iso_639_language_code2
  text                   = _text
