module Descriptor.CarouselCompatibleComposite (
  Data,Class(..)
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import qualified Descriptor.LangCode as LangCode
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor as SubDescriptor

class (Base.Class a) => Class a where
  sub_descriptors :: a -> [SubDescriptor.Data]

data Data = MkData {
  _header            :: Header.Data,
  _sub_descriptors   :: Vector SubDescriptor.Data
  } deriving (Show)

instance Base.Class Data where

instance Class Data where
  sub_descriptors = toList . _sub_descriptors
