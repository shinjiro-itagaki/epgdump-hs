{-# LANGUAGE FlexibleInstances #-}

module SITables.Common(
  HasDescriptors(..)
  ,Schedule(..)
  ,MatchPID(..)
  ,MatchTableID(..)
  ,PID
  ,TableID
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Descriptor
import Common(EmptyExist(..))
import Parser(ParseConditionSymbol(..),FromValueCache(..),ValueCache)

class HasDescriptors a where
  descriptors :: a -> [Descriptor.Data]

class Schedule a where
  start_time :: a -> Word64
  duration   :: a -> Word32

type PID = Word64
type TableID = Word32

class MatchPID a where
  match_pid :: a -> PID -> Bool

class MatchTableID a where
  match_table_id :: a -> TableID -> Bool

instance MatchPID [PID] where
  match_pid (x:[]) y = False
  match_pid (x:xs) y = if x == y then True else match_pid xs y

instance MatchTableID [TableID] where
  match_table_id (x:[]) y = False
  match_table_id (x:xs) y = if x == y then True else match_table_id xs y
