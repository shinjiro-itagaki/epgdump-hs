
module Descriptor.ComponentGroup.Item (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.CountryCode as CountryCode
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.ComponentGroup.CA_Unit as CA_Unit

class Class a where
  component_group_id :: a -> Word8
  num_of_CA_unit     :: a -> Word8
  ca_units           :: a -> [CA_Unit.Data]
  total_bit_rate     :: a -> Maybe Word8 -- total_bit_rate_flag == True
  text_length        :: a -> Word8
  text               :: a -> String

data Data = MkData {
  _component_group_id :: Word8,
  _num_of_CA_unit     :: Word8,
  _ca_units           :: Vector CA_Unit.Data,
  _total_bit_rate     :: Maybe Word8, -- total_bit_rate_flag == True
  _text_length        :: Word8,
  _text               :: String
  }

instance Class Data where
  component_group_id = _component_group_id
  num_of_CA_unit     = _num_of_CA_unit
  ca_units           = toList . _ca_units
  total_bit_rate     = _total_bit_rate
  text_length        = _text_length
  text               = _text
