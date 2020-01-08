module Descriptor.StreamIdentifier (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..),HasComponentTag(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasComponentTag a) => Class a where
--  component_tag :: a -> Word8
  
data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _component_tag     :: Word8
  }
  
instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where

instance HasComponentTag Data where
  component_tag = _component_tag
  
instance Class Data where
