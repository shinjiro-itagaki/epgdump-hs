module Descriptor.CarouselCompatibleComposite.ProviderPrivate (
  Class(..)
  ,Data
  ) where
import Descriptor.CarouselCompatibleComposite.Common(SubDescriptor(..))
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (SubDescriptor a) => Class a where
  private_scope_type :: a -> Word8
  scope_identifier :: a -> Word32
  private_bytes :: a -> [Word8]


data Data = MkData {
  _pp_descriptor_tag     :: Word8,
  _pp_descriptor_length  :: Word8,
  _pp_private_scope_type :: Word8,
  _pp_scope_identifier   :: Word32,
  _pp_private_bytes      :: [Word8]
  }

instance Descriptor Data where
  descriptor_tag = _pp_descriptor_tag
  descriptor_length = _pp_descriptor_length

instance SubDescriptor Data

instance Class Data where
  private_scope_type = _pp_private_scope_type
  scope_identifier   = _pp_scope_identifier
  private_bytes      = _pp_private_bytes
