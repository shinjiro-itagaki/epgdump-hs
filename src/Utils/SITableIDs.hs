{-# LANGUAGE FlexibleInstances #-}

module Utils.SITableIDs where
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Utils.EmptyExist as EmptyExist
import Common

class Class a where
  pids      :: a -> PIDs
  table_ids :: a -> [TableID]
  
  -- (==.=) :: (Class b) => a -> b -> Bool
  -- (==.=) x y = ((pids x) `matchPID` (pid y)) && ((table_ids x) ==|= (table_id y))

  -- (=.==) :: (Class b) => b -> a -> Bool
  -- (=.==) x y = y ==.= x 
  
  -- (/==.=) :: (Class b) => a -> b -> Bool  
  -- (/==.=) x y = not $ x ==.= y

  -- (/=.==) :: (Class b) => b -> a -> Bool
  -- (/=.==) x y = not $ x =.== y
