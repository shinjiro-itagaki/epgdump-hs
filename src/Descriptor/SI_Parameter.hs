module Descriptor.SI_Parameter where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => SI_Parameter a where
  parameter_version :: a -> Word8
  update_time :: a -> Word16
  table_descriptions :: a -> [SI_TableDescription]


data SI_TableDescription = MkSI_TableDescription{
  table_id :: Word8,
  table_description_length :: Word8,
  table_description_bytes :: [Word8]
  }
