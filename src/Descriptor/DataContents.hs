module Descriptor.DataContents (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasISO_639_LanguageCode(..), HasText(..), HasTextAndLen(..), HasSelector(..), HasDataComponentID(..),Descriptor(..),LangCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasISO_639_LanguageCode a, HasTextAndLen a, HasSelector a, HasDataComponentID a) => Class a where
--  data_component_id :: a -> Word16
  entry_component   :: a -> Word8
--  selector_length   :: a -> Word8
--  selector_bytes    :: a -> [Word8]
  num_of_component_ref :: a -> Word8
  component_ref :: a -> [Word8]
--  iso_639_language_code :: a -> LangCode
--  text_length :: a -> Word8
--  text :: a -> String

data Data = MkData {
  _descriptor_tag        :: Word8,
  _descriptor_length     :: Word8,
  _data_component_id     :: Word16,
  _entry_component       :: Word8,
  _selector_length       :: Word8,
  _selector_bytes        :: [Word8],
  _num_of_component_ref  :: Word8,
  _component_ref         :: [Word8],
  _iso_639_language_code :: LangCode,
  _text_length           :: Word8,
  _text                  :: String
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
  
instance HasSelector Data where
  selector_length = _selector_length
  selector_bytes  = _selector_bytes
  
instance HasDataComponentID Data where
  data_component_id = _data_component_id
  
instance Class Data where
  entry_component      = _entry_component
  num_of_component_ref = _num_of_component_ref
  component_ref        = _component_ref
