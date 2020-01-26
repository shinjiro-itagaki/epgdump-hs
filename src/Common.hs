{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Common(
  module Data.Word
  ,module Data.Bits
  ,module Data.Int
  ,module Data.Vector
  ,module Data.Maybe
  ,ByteString
  ,BytesLen
  ,BitsLen
  ,PID
  ,TableID
  ,PIDs(..)
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy as BS
import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits,shiftR,shiftL,(.&.),(.|.))
import Data.Int(Int64)
import Data.Vector(Vector,snoc)
import Data.Maybe(fromMaybe)
import Control.Applicative((<|>))

type ByteString = BS.ByteString
type BytesLen = Word64
type BitsLen  = Word64
type PID = Word64
type TableID = Word32
data PIDs = MkPIDs [PID] | MkExcludePIDs [PID] deriving (Eq,Show)

-- class PID_And_TableID a where
--   pid      :: a -> PID
--   table_id :: a -> TableID

-- instance PID_And_TableID (PID,TableID) where
--   pid = fst
--   table_id = snd

-- instance PID_And_TableID (TableID,PID) where
--   pid = snd
--   table_id = fst
