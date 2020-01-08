module Descriptor.SI_Parameter (
  Class(..)
  ,Data
  ,TableDescription
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  parameter_version  :: a -> Word8
  update_time        :: a -> Word16
  table_descriptions :: a -> [TableDescription]


data Data = MkData {
  _descriptor_tag     :: Word8,
  _descriptor_length  :: Word8,
  _parameter_version  :: Word8,
  _update_time        :: Word16,
  _table_descriptions :: [TableDescription]
  }

data TableDescription = MkTableDescription{
  table_id :: Word8,
  table_description_length :: Word8,
  table_description_bytes :: [Word8]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  parameter_version  = _parameter_version
  update_time        = _update_time
  table_descriptions = _table_descriptions
  
