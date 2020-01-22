module Descriptor.CarouselCompatibleComposite.SubDescriptor.ProviderPrivate (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)

import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base

class (Base.Class a) => Class a where
  private_scope_type :: a -> Word8
  scope_identifier   :: a -> Word32
  private_bytes      :: a -> [Word8]

data Data = MkData {
  _header                :: Header.Data,
  _pp_private_scope_type :: Word8,
  _pp_scope_identifier   :: Word32,
  _pp_private_bytes      :: [Word8]
  } deriving (Show)

instance Header.Class Data where
  setHeader x h = x { _header = h }
  header = _header

instance Base.Class Data where

instance Class Data where
  private_scope_type = _pp_private_scope_type
  scope_identifier   = _pp_scope_identifier
  private_bytes      = _pp_private_bytes
