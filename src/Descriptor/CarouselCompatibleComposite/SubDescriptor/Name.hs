module Descriptor.CarouselCompatibleComposite.SubDescriptor.Name where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base

class (Base.Class a) => Class a where
  name :: a -> String

data Data = MkData {
  _header :: Header.Data,
  _name   :: String  
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  
instance Class Data where
  name = _name
