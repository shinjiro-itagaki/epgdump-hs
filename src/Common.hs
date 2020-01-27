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
import Data.Bits(Bits,shiftR,shiftL,(.&.),(.|.),bitSizeMaybe)
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
