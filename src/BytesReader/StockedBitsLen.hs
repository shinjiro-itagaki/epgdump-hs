{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BytesReader.StockedBitsLen where
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(pack,unpack,null,uncons,empty)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import Data.Char(chr)
import Common(ByteString,BytesLen)
import Data.ByteString.Lazy(head,last,unpack)

data Data = Zero | One | Two | Three | Four | Five | Six | Seven | All deriving (Enum,Bounded,Eq,Ord)

stockedValueMask :: (Num a) => Data -> a
stockedValueMask x =
  case x of
    Zero  -> 0x00
    One   -> 0x01
    Two   -> 0x03
    Three -> 0x07
    Four  -> 0x0F
    Five  -> 0x1F
    Six   -> 0x3F
    Seven -> 0x7F
    All   -> 0xFF

stockedBitsNumLen :: (Num a) => Data -> a
stockedBitsNumLen x =
  case x of
    Zero  -> 0
    One   -> 1
    Two   -> 2
    Three -> 3
    Four  -> 4
    Five  -> 5
    Six   -> 6
    Seven -> 7
    All   -> 8

numToStockedBitsLen :: (Integral a) => a -> Data
numToStockedBitsLen i
  | i < 0 = Zero
  | i > 8 = All
  | otherwise =
    case i of
      0 -> Zero
      1 -> One
      2 -> Two
      3 -> Three
      4 -> Four
      5 -> Five
      6 -> Six
      7 -> Seven
      8 -> All

shipStockedBitsResult :: Data -> Data -> Word8 -> (Word8,Data) -- 左は得られる値、右は残るビット数
shipStockedBitsResult rest n val
  | rest < n = shipStockedBitsResult rest rest val
  | otherwise =
    let val'  = val .&. (stockedValueMask rest)
        rest' = fromEnum n
        n'    = fromEnum n
        slen' = rest' - n'
    in (val' `shiftR` slen', toEnum $ slen')
