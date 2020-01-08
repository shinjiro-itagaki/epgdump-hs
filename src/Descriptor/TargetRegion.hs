module Descriptor.TargetRegion where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => TargetRegion a where
  region_spec_type :: a -> Word8 -- only 0x01
  specs :: a -> Maybe [TargetRegionSpecData]

class TargetRegionSpec a where
  pefecture_bitmap :: a -> Word64

data TargetRegionSpecData = MkTargetRegionSpec {
  _pefecture_bitmap :: Word64
  }

data TargetRegionData = MkTargetRegionData {
  _target_region_descriptor_tag :: Word8,
  _target_region_descriptor_length :: Word8,
  _target_region_spec_type :: Word8, -- only 0x01
  _target_specs :: Maybe [TargetRegionSpecData]  
  }

instance Descriptor TargetRegionData where
  descriptor_tag = _target_region_descriptor_tag
  descriptor_length = _target_region_descriptor_length

-- instance Base TargetRegionData where

-- instance TargetRegion TargetRegionData where
--   region_spec_type = _target_region_spec_type
--   specs = _target_specs

instance TargetRegionSpec TargetRegionSpecData where
  pefecture_bitmap = _pefecture_bitmap

