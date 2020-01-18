module Descriptor.CountryCode where

import Data.Word(Word64, Word32, Word16, Word8)

type Data = (Char,Char,Char)

class Has a where
  country_code :: a -> Data
