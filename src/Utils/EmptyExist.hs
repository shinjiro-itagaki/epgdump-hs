{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Utils.EmptyExist where
import Common
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy as BS

class Class a where
  mkEmpty :: a
instance Class ByteString where
  mkEmpty = BS.empty
instance Class Word64 where
  mkEmpty = 0
instance Class Word32 where
  mkEmpty = 0  
instance Class Word16 where
  mkEmpty = 0  
instance Class Word8  where
  mkEmpty = 0
instance Class (Bool,Bool) where
  mkEmpty = (False,False)
instance Class Char where
  mkEmpty = '\0'
instance Class Bool where
  mkEmpty = False

