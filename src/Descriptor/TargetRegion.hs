module Descriptor.TargetRegion (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.AreaCode as AreaCode
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.TargetRegion.Spec as Spec
import Data.Vector(Vector,toList)
import Data.Bits((.&.))

data RegionSpecType =
  Reservation -- others
  | BS_PrefSpec  -- 0x01 Region designation of prefecture for BS digital

class Base.Class a => Class a where
  region_spec_type :: a -> RegionSpecType -- only 0x01
  specs            :: a -> [Spec.Data] -- [56]

data Data = MkData {
  _header            :: Header.Data, 
  _region_spec_type  :: Word8, -- only 0x01 -- 8
  _specs             :: Vector Spec.Data
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  region_spec_type x = if (_region_spec_type x) .&. 0x01 > 0 then BS_PrefSpec else Reservation
  specs            = toList . _specs

