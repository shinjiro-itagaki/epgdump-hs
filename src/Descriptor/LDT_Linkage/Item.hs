module Descriptor.LDT_Linkage.Item (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import Data.Vector(Vector,empty,toList,snoc)

class Class a where
  description_id      :: a -> Word16 -- 16
  reserved_future_use :: a -> Word8 -- 4
  description_type    :: a -> Word8 -- 4
  user_defined        :: a -> Word8 -- 8  

data Data = MkData {
  _description_id      :: Word16, -- 16
  _reserved_future_use :: Word8, -- 4
  _description_type    :: Word8, -- 4
  _user_defined        :: Word8 -- 8
  }

instance Class Data where
  description_id      = _description_id
  reserved_future_use = _reserved_future_use
  description_type    = _description_type
  user_defined        = _user_defined
