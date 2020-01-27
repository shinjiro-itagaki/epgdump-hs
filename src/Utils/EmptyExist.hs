{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Utils.EmptyExist where
import Common
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

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
instance Class Char where
  mkEmpty = '\0'
instance Class Bool where
  mkEmpty = False
instance Class (Vector a) where
  mkEmpty = V.empty
instance (Class a, Class b) => Class (a,b) where
  mkEmpty = (mkEmpty,mkEmpty)
instance (Class a, Class b, Class c) => Class (a,b,c) where
  mkEmpty = (mkEmpty,mkEmpty,mkEmpty)
instance (Class a, Class b, Class c,Class d) => Class (a,b,c,d) where
  mkEmpty = (mkEmpty,mkEmpty,mkEmpty,mkEmpty)

