-- 6.2.45
module Descriptor.ContentAvailability (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import Data.Vector(Vector,empty,toList,snoc)

class Base.Class a => Class a where
  copy_restriction_mode  :: a -> Bool
  image_constraint_token :: a -> Bool
  retention_mode         :: a -> Bool
  retention_state        :: a -> Word8
  encryption_mode        :: a -> Bool
  reserved_future_uses   :: a -> [Word8] -- 8 * N  

data Data = MkData {
  _header                 :: Header.Data, 
  _copy_restriction_mode  :: Bool,
  _image_constraint_token :: Bool,
  _retention_mode         :: Bool,
  _retention_state        :: Word8,
  _encryption_mode        :: Bool,
  _reserved_future_uses   :: Vector Word8
  }

instance Base.Class Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  copy_restriction_mode  = _copy_restriction_mode
  image_constraint_token = _image_constraint_token
  retention_mode         = _retention_mode
  retention_state        = _retention_state
  encryption_mode        = _encryption_mode
  reserved_future_uses   = toList . _reserved_future_uses
