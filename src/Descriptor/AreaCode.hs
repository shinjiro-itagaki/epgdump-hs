module Descriptor.AreaCode where

import Data.Word(Word64, Word32, Word16, Word8)

type Data = Word16

class Has a where
  area_code :: a -> Data
