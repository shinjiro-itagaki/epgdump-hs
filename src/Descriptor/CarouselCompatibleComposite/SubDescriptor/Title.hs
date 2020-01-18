module Descriptor.CarouselCompatibleComposite.SubDescriptor.Title (
  Class(..)
  ,Data
  ) where
import qualified Descriptor.LangCode as LangCode
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base

class (Base.Class a, Header.Class a) => Class a where
  text      :: a -> String
  lang_code :: a -> LangCode.Data
  
data Data = MkData {
  _title_descriptor_tag    :: Word8,
  _title_descriptor_length :: Word8,
  _title_text              :: String,
  _title_iso_639_language_code :: LangCode.Data
  }
 
instance Header.Class Data where
  descriptor_tag    = _title_descriptor_tag
  descriptor_length = _title_descriptor_length

instance Base.Class Data where
--  
  
instance Class Data where
  text      = _title_text  
  lang_code = _title_iso_639_language_code

