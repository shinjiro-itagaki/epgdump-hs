module Descriptor.HierarchicalTransmission (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

class (Base.Class a) => Class a where
  quality_level :: a -> Bool
  reference_pid :: a -> Word16

data Data = MkData {
  _header            :: Header.Data,
  _quality_level     :: Bool,
  _reference_pid     :: Word16
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where

instance Class Data where  
  quality_level = _quality_level
  reference_pid = _reference_pid
