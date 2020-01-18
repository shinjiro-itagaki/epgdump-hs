module Descriptor.SI_Parameter (
  Class(..)
  ,Data
  ,TableDescription
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header

class Base.Class a => Class a where
  parameter_version  :: a -> Word8
  update_time        :: a -> Word16
  table_descriptions :: a -> [TableDescription]


data Data = MkData {
  _header             :: Header.Data,
  _parameter_version  :: Word8,
  _update_time        :: Word16,
  _table_descriptions :: [TableDescription]
  }

data TableDescription = MkTableDescription{
  table_id :: Word8,
  table_description_length :: Word8,
  table_description_bytes :: [Word8]
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  parameter_version  = _parameter_version
  update_time        = _update_time
  table_descriptions = _table_descriptions
  
