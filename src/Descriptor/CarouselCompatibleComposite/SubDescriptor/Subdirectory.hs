module Descriptor.CarouselCompatibleComposite.SubDescriptor.Subdirectory where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base

class (Base.Class a) => Class a where
  subdirectory_path :: a -> String

data Data = MkData {
  _header            :: Header.Data,
  _subdirectory_path :: String
  }

instance Header.Class Data where
  setHeader x h = x {_header = h}  
  header = _header

instance Base.Class Data where
  
instance Class Data where
  subdirectory_path = _subdirectory_path
