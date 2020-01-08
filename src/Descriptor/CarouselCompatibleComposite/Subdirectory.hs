module Descriptor.CarouselCompatibleComposite.Subdirectory where
import Descriptor.CarouselCompatibleComposite.Common(SubDescriptor(..))
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (SubDescriptor a) => Class a where
  subdirectory_path :: a -> String

data Data = MkData {
  _sd_descriptor_tag    :: Word8,
  _sd_descriptor_length :: Word8,
  _subdirectory_path    :: String
  }

instance Descriptor Data where
  descriptor_tag = _sd_descriptor_tag
  descriptor_length = _sd_descriptor_length


instance SubDescriptor Data where

instance Class Data where
  subdirectory_path = _subdirectory_path
