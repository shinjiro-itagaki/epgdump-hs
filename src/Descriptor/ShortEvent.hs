module Descriptor.ShortEvent (
  Class(..)
  ,Data
  ) where
-- import Descriptor.Common(HasISO_639_LanguageCode(..),HasTextAndLen(..),LangCode,HasTextAndLen(..), HasText(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Utils.LangCode as LangCode
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header

-- class (Base.Class a, HasISO_639_LanguageCode a, HasTextAndLen a) => Class a where
class (Base.Class a)  => Class a where
  iso_639_language_code :: a -> LangCode.Data
  event_name_length :: a -> Word8
  event_name        :: a -> String
  text_length :: a -> Word8
  text :: a -> String

data Data = MkData {
  _header            :: Header.Data,
  _iso_639_language_code :: LangCode.Data,  
  _event_name_length :: Word8,
  _event_name        :: String,
  _text_length       :: Word8,
  _text              :: String
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  
instance Class Data where
  iso_639_language_code = _iso_639_language_code
  event_name_length     = _event_name_length
  event_name            = _event_name
  text_length           = _text_length
  text                  = _text
