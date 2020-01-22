
-- 6.2.37
module Descriptor.ComponentGroup (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import Data.Vector(Vector,empty,toList,snoc)

import qualified Descriptor.ComponentGroup.Item as Item

class (Base.Class a) => Class a where
  component_group_type :: a -> Word8
  total_bit_rate_flag  :: a -> Bool
  num_of_group         :: a -> Word8
  items                :: a -> [Item.Data]

data Data = MkData {
  _header               :: Header.Data,
  _component_group_type :: Word8,
  _total_bit_rate_flag  :: Bool,
  _num_of_group         :: Word8,
  _items                :: Vector Item.Data  
  } deriving (Show)

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  component_group_type = _component_group_type
  total_bit_rate_flag  = _total_bit_rate_flag
  num_of_group         = _num_of_group 
  items                = toList . _items
