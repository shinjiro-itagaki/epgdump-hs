{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Common where
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(pack,unpack,null,uncons,empty)
import Data.Bits((.&.),shiftL,shiftR)
import Data.Char(chr)
import qualified Data.ByteString.Lazy as BS

type ByteString = BS.ByteString

type BytesLen = Word64
type BitsLen  = Word64

type PID = Word64
type TableID = Word32

data PIDs = MkPIDs [PID] | MkExcludePIDs [PID] deriving (Eq)

class EmptyExist a where
  mkEmpty :: a
instance EmptyExist ByteString where
  mkEmpty = Data.ByteString.Lazy.empty
instance EmptyExist Word64 where
  mkEmpty = 0
instance EmptyExist Word32 where
  mkEmpty = 0  
instance EmptyExist Word16 where
  mkEmpty = 0  
instance EmptyExist Word8  where
  mkEmpty = 0
instance EmptyExist (Bool,Bool) where
  mkEmpty = (False,False)

class (Eq a) => Matcher a where
  -- please implement
  (=|==) :: a -> [a] -> Bool
  (=|==) x []     = False
  (=|==) x (y:[]) = (x == y)
  (=|==) x (y:ys) = (x == y) || x =|== ys
  --

  (/=|==) :: a -> [a] -> Bool
  (/=|==) x y = not $  x =|== y
  
  (==|=) :: [a] -> a -> Bool
  (==|=) xs y = y =|== xs

  (/==|=) :: [a] -> a -> Bool
  (/==|=) x y = not $ x ==|= y

  (==|==) :: [a] -> [a] -> Bool
  (==|==) []     ys = False
  (==|==) (x:[]) ys = x =|== ys
  (==|==) (x:xs) ys = x =|== ys || xs ==|== ys

  (/==|==) :: [a] -> [a] -> Bool
  (/==|==) x y = not $ x ==|== y

instance Matcher PID
instance Matcher TableID

matchPID :: PIDs -> PID -> Bool
matchPID (MkPIDs        xs) y = xs ==|= y
matchPID (MkExcludePIDs []) y = True
matchPID (MkExcludePIDs xs) y = xs /==|= y

class PID_And_TableID a where
  pid      :: a -> PID
  table_id :: a -> TableID

instance PID_And_TableID (PID,TableID) where
  pid = fst
  table_id = snd

instance PID_And_TableID (TableID,PID) where
  pid = snd
  table_id = fst
