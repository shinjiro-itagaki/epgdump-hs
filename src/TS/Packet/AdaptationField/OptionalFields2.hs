{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.AdaptationField.OptionalFields2 where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import Data.ByteString.Lazy(ByteString)

class Class a where
  ltw_valid_flag :: a -> Bool -- 1
  ltw_offset     :: a -> Word16 -- 15
  __             :: a -> (Bool,Bool) -- 2
  piecewise_rate :: a -> Word32 -- 22
  splice_type    :: a -> Word8 -- 4
  dts_next_au    :: a -> Word64 -- 33


data Data = MkData {
  _ltw_valid_flag :: Bool, -- 1
  _ltw_offset     :: Word16, -- 15
  ___             :: (Bool,Bool), -- 2
  _piecewise_rate :: Word32, -- 22
  _splice_type    :: Word8, -- 4
  _dts_next_au    :: Word64 -- 33
  }
