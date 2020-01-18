module Descriptor.CarouselCompatibleComposite.SubDescriptor.StoreRoot where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base


class (Base.Class a) => Class a where
  update_type     :: a -> Bool
  reserved        :: a -> Word8
  store_root_path :: a -> String


data Data = MkData {
  _header          :: Header.Data,
  _update_type     :: Bool,
  _reserved        :: Word8,
  _store_root_path :: String
  }

instance Header.Class Data where
  header = _header
  setHeader x h = x { _header = h }

instance Base.Class Data where

instance Class Data where
  update_type     = _update_type
  reserved        = _reserved
  store_root_path = _store_root_path
