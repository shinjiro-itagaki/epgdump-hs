module Descriptor.CarouselCompatibleComposite.StoreRoot where
import Descriptor.CarouselCompatibleComposite.Common(SubDescriptor(..))
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (SubDescriptor a) => Class a where
  update_type :: a -> Bool
-- reserved :: Word8
  store_root_path :: a -> String


data Data = MkData {
  _sr_update_type       :: Bool,
  _sr_descriptor_tag    :: Word8,
  _sr_descriptor_length :: Word8,
  _sr_store_root_path   :: String
  }

instance Descriptor Data where
  descriptor_tag = _sr_descriptor_tag
  descriptor_length = _sr_descriptor_length

instance SubDescriptor Data

instance Class Data where
  update_type = _sr_update_type
  store_root_path = _sr_store_root_path
