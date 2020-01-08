module Descriptor.NetworkName (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..),HasName(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasName a) => Class a where

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _name              :: String
  }

  
instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance HasName Data where
  name = _name

instance Class Data where
