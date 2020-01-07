{-# LANGUAGE FlexibleInstances #-}

module Lib where

import System.IO(
  Handle
  ,hIsEOF
  ,SeekMode(..)
  ,hSeek
  ,hFileSize
  )
  
import Data.ByteString(hGet,hGetSome,length, ByteString,unpack, take, pack, append,map, head)
import Data.Vector(Vector,empty,snoc,length)
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Char8(pack)
import Data.Bits((.&.),(.|.),shiftL,shiftR,Bits)
import Data.Char(intToDigit,digitToInt)
