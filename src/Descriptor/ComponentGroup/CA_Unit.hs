module Descriptor.ComponentGroup.CA_Unit (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.CountryCode as CountryCode
import Data.Vector(Vector,empty,toList,snoc)

class Class a where
  ca_unit_id       :: a -> Word8
  num_of_component :: a -> Word8
  component_tags   :: a -> [Word8]

data Data = MkData {
  _ca_unit_id       :: Word8,
  _num_of_component :: Word8,
  _component_tags   :: Vector Word8
  } deriving (Show)

instance Class Data where
  ca_unit_id       = _ca_unit_id
  num_of_component = _num_of_component
  component_tags   = toList . _component_tags
