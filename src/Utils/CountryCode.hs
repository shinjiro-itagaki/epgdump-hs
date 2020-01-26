{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.CountryCode where

import Data.Word(Word64, Word32, Word16, Word8)
import qualified Utils.ToString as ToString

type Data = (Char,Char,Char)

class Class a where
  country_code :: a -> Data

--instance ToString.Class Data where
--  toString (x,y,z) = [x,y,z]
