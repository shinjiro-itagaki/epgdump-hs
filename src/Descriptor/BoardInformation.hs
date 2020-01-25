module Descriptor.BoardInformation (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Utils.LangCode as LangCode
import Data.Vector(Vector,empty,toList,snoc)

class (Base.Class a) => Class a where
  title        :: a -> String
  title_length :: a -> Word8
  text         :: a -> String
  text_length  :: a -> Word8

data Data = MkData {
  _header       :: Header.Data,
  _text         :: String,
  _text_length  :: Word8,
  _title        :: String,
  _title_length :: Word8  
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)
  
instance Class Data where
  text         = _text
  text_length  = _text_length
  title        = _title
  title_length = _title_length
