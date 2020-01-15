{-# LANGUAGE FlexibleInstances #-}

module SITables.Common(
  HasDescriptors(..)
  ,Schedule(..)
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Descriptor
import Common(EmptyExist(..))
import Parser(HasParser(..))
import Common(PID,PIDs(..),TableID,(==|=),PID_And_TableID(..),matchPID)

class HasDescriptors a where
  descriptors :: a -> [Descriptor.Data]

class Schedule a where
  start_time :: a -> Word64
  duration   :: a -> Word32

class SITableIDs a where
  pids      :: a -> PIDs
  table_ids :: a -> [TableID]
  
  (==.=) :: (PID_And_TableID b) => a -> b -> Bool
  (==.=) x y = ((pids x) `matchPID` (pid y)) && ((table_ids x) ==|= (table_id y))

  (=.==) :: (PID_And_TableID b) => b -> a -> Bool
  (=.==) x y = y ==.= x 
  
  (/==.=) :: (PID_And_TableID b) => a -> b -> Bool  
  (/==.=) x y = not $ x ==.= y

  (/=.==) :: (PID_And_TableID b) => b -> a -> Bool
  (/=.==) x y = not $ x =.== y

data SITableIDsData = MkSITableIDsData PIDs [TableID]

instance SITableIDs SITableIDsData where
  pids      (MkSITableIDsData _pids _     ) = _pids
  table_ids (MkSITableIDsData _ _table_ids) = _table_ids
