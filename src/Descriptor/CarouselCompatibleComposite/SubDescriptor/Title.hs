module Descriptor.CarouselCompatibleComposite.SubDescriptor.Title (
  Class(..)
  ,Data
  ) where
import qualified Utils.LangCode as LangCode
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base

class (Base.Class a, Header.Class a) => Class a where
  text      :: a -> String
  lang_code :: a -> LangCode.Data
  
data Data = MkData {
  _header                :: Header.Data,  
  _text                  :: String,
  _iso_639_language_code :: LangCode.Data
  } deriving (Show)
 
instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  
  
instance Class Data where
  text      = _text  
  lang_code = _iso_639_language_code

