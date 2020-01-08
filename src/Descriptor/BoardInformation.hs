module Descriptor.BoardInformation (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasTitle(..),HasText(..),HasTextAndLen(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasTitle a, HasTextAndLen a) => Class a where
--  title :: a -> String
--  title_length :: a -> Word8

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _text              :: String,
  _text_length       :: Word8,
  _title             :: String,
  _title_length      :: Word8  
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)
  
instance HasText Data where
  text = _text

instance HasTextAndLen Data where
  text_length = _text_length

instance HasTitle Data where
  title        = _title
  title_length = _title_length

instance Class Data where
