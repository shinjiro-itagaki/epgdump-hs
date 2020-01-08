module Descriptor.ComponentGroup (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasText(..),HasTextAndLen(..),HasComponent(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a) => Class a where
  component_group_type :: a -> Word8
  total_bit_rate_flag  :: a -> Bool
  num_of_group         :: a -> Word8
  groups               :: a -> [Item]

data Data = MkData {
  _descriptor_tag       :: Word8,
  _descriptor_length    :: Word8,
  _component_group_type :: Word8,
  _total_bit_rate_flag  :: Bool,
  _num_of_group         :: Word8,
  _groups               :: [Item]  
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  component_group_type = _component_group_type
  total_bit_rate_flag  = _total_bit_rate_flag
  num_of_group         = _num_of_group 
  groups               = _groups

data CA_Unit = MkCA_Unit {
  ca_unit_id       :: Word8,
  num_of_component :: Word8,
  component_tags   :: [Word8]
  }

data Item = MkItem {
  component_group_id :: Word8,
  num_of_ca_unit     :: Word8,
  ca_units           :: [CA_Unit],
  total_bit_rate     :: Maybe Word8,
  _text_length       :: Word8,
  _text              :: String
  }

instance HasText Item where
  text = _text

instance HasTextAndLen Item where
  text_length = _text_length
