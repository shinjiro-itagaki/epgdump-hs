module Descriptor.LDT_Link where
import Descriptor.Common(Base,TOS,HasServiceID(..),HasUserDefined(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, TOS a) => LDT_Link a where
  original_service_id :: a -> Word16
  original_service_id = service_id
  descriptions :: a -> [LDT_LinkDesc]

data LDT_LinkDesc = MkLDT_LinkDesc {
  description_id :: Word16,
--  reserved_future_use :: Word8,
  description_type :: Word8,
  _LDT_LinkDesc_user_defined :: Word8
  }

instance HasUserDefined LDT_LinkDesc where
  user_defined = _LDT_LinkDesc_user_defined
