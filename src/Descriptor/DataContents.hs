module Descriptor.DataContents (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.EmergencyInformation.Item as Item
import qualified Descriptor.Selector as Selector
import qualified Utils.LangCode as LangCode

class (Base.Class a, Selector.Class a) => Class a where
  data_component_id     :: a -> Word16
  entry_component       :: a -> Word8
--  selector_length     :: a -> Word8
--  selector_bytes      :: a -> [Word8]
  num_of_component_ref  :: a -> Word8
  component_ref         :: a -> [Word8]
  iso_639_language_code :: a -> LangCode.Data
  text_length           :: a -> Word8
  text                  :: a -> String

data Data = MkData {
  _header                :: Header.Data,
  _data_component_id     :: Word16,
  _entry_component       :: Word8,
  _selector_length       :: Word8,
  _selector_bytes        :: ByteString,
  _num_of_component_ref  :: Word8,
  _component_ref         :: [Word8],
  _iso_639_language_code :: LangCode.Data,
  _text_length           :: Word8,
  _text                  :: String
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Selector.Class Data where
  selector_length = _selector_length
  selector_bytes  = _selector_bytes
  
instance Class Data where
  iso_639_language_code = _iso_639_language_code  
  data_component_id    = _data_component_id
  entry_component      = _entry_component
  num_of_component_ref = _num_of_component_ref
  component_ref        = _component_ref
  text_length          = _text_length
  text                 = _text
