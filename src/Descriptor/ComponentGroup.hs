module Descriptor.ComponentGroup where
import Descriptor.Common(Base,HasText(..),HasTextAndLen(..),HasComponent)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasText a, HasComponent a) => ComponentGroup a where
  component_group_type :: a -> Word8
  total_bit_rate_flag :: a -> Bool
  num_of_group :: a -> Word8
  groups :: a -> [ComponentGroupData]

data CA_Unit = MkCA_Unit {
  ca_unit_id :: Word8,
  num_of_component :: Word8,
  component_tags :: [Word8]
  }

data ComponentGroupData = MkComponentGroupData {
  component_group_id :: Word8,
  num_of_ca_unit :: Word8,
  ca_units :: [CA_Unit],
  total_bit_rate :: Maybe Word8,
  _component_group_data_text_length :: Word8,
  _component_group_data_text :: String
  }

instance HasText ComponentGroupData where
  text = _component_group_data_text

instance HasTextAndLen ComponentGroupData where
  text_length = _component_group_data_text_length
