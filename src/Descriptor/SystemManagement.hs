module Descriptor.SystemManagement (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header


class Base.Class a => Class a where
  broadcasting_flag                  :: a -> Word8
  broadcasting_identifier            :: a -> Word8
  additional_broadcasting_identifier :: a -> Word8
  additional_identification_info     :: a -> [Word8]

data Data = MkData {
  _header                             :: Header.Data,
  _broadcasting_flag                  :: Word8,
  _broadcasting_identifier            :: Word8,
  _additional_broadcasting_identifier :: Word8,
  _additional_identification_info     :: [Word8]
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  broadcasting_flag                  = _broadcasting_flag
  broadcasting_identifier            = _broadcasting_identifier
  additional_broadcasting_identifier = _additional_broadcasting_identifier
  additional_identification_info     = _additional_identification_info
