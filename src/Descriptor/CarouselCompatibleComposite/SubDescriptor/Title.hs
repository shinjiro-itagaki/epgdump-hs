module Descriptor.CarouselCompatibleComposite.SubDescriptor.Title (
  Class(..)
  ,Data
  ) where
import qualified Descriptor.LangCode as LangCode
import Descriptor.Common(HasText(..),HasName(..),HasText(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base

class (Base.Class a,HasText a) => Class a where
  iso_639_language_code :: a -> LangCode.Data
  
data Data = MkData {
  _title_descriptor_tag    :: Word8,
  _title_descriptor_length :: Word8,
  _title_text              :: String,
  _title_iso_639_language_code :: LangCode.Data
  }
 
instance HasText Data where
  text = _title_text

instance Header.Class Data where

instance Base.Class Data where
--  
  
instance Class Data where
  iso_639_language_code = _title_iso_639_language_code

