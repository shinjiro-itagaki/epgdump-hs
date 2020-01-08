module Descriptor.TargetRegion (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  region_spec_type :: a -> Word8 -- only 0x01
  specs            :: a -> Maybe [SpecData]

class SpecClass a where
  pefecture_bitmap :: a -> Word64

data SpecData = MkSpecData {
  _pefecture_bitmap :: Word64
  }

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _region_spec_type  :: Word8, -- only 0x01
  _specs             :: Maybe [SpecData]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where

instance Class Data where
  region_spec_type = _region_spec_type
  specs            = _specs

instance SpecClass SpecData where
  pefecture_bitmap = _pefecture_bitmap

