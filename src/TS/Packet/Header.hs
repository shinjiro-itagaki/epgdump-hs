{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.Header where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import Data.ByteString.Lazy(ByteString,head,unpack)
import Common(EmptyExist(..))

data ScrambleControl =
  NoScramble -- '00'
  | UndefScramble -- '01' 
  | EvenKey -- '10'
  | OddKey -- '11'

-- adaptation_field_control
-- 上1ビットが立っている(2 or 3)場合はadaptation_fieldがある
-- 下1ビットが立っている(1 or 3)場合はpayloadがある（ヘッダに続いて直ちにpayloadが始まる）
-- 32bit
class (EmptyExist a) => Class a where
--  mkEmpty :: a
  
  header                       :: a -> Data
  -- TSパケット内のビットエラーの有無を示すフラグで、trueの場合、少なくとも1ビットの訂正不可能なエラーがTSパケットに存在することを示す
  transport_error_indicator    :: a -> Bool -- 1 bits
  transport_error_indicator = transport_error_indicator . header
  
  -- trueのとき本TSパケットのペイロードの開始点がPESパケットの開始点、又はポインタであることを示す
  payload_unit_start_indicator :: a -> Bool -- 1 bits
  payload_unit_start_indicator = payload_unit_start_indicator . header

  -- 同一のPIDを持つパケットの中での優先度を示すフラグで、trueが優先を示すものとする。
  transport_priority           :: a -> Bool -- 1 bits
  transport_priority = transport_priority . header
  
  pid                          :: a -> Word16 -- 13 bits
  pid = pid . header

  -- TSパケットのペイロードのスクランブルモードを識別するのに使用する領域
  transport_scrambling_control :: a -> ScrambleControl -- 2 bits
  transport_scrambling_control x =
    case transport_scrambling_control_raw $ header x of
      0 -> NoScramble
      1 -> UndefScramble
      2 -> EvenKey
      3 -> OddKey
      otherwise -> UndefScramble

  adaptation_field_control     :: a -> Word8 -- 2 bits
  adaptation_field_control = adaptation_field_control . header
  
  continuity_counter           :: a -> Word8 -- 4 bits
  continuity_counter = continuity_counter . header

  -- '10' or '11'
  has_adaptation_field         :: a -> Bool
  has_adaptation_field         = (> 0) . (.&. 0x02) . adaptation_field_control

  -- '01' or '11'
  has_payload                  :: a -> Bool
  has_payload                  = (> 0) . (.&. 0x01) . adaptation_field_control

  transport_scrambling_control_raw :: a -> Word8
  transport_scrambling_control_raw = transport_scrambling_control_raw . header
  
                                   
type Data = (Word8,(Word8,Word8))

--readNum :: (Integral a, Bits a, Num b) => a -> a -> b
--readNum mask x = fromInteger $ toInteger $ (x .&. mask) `shiftR` (sftcount mask 0)
--  where
--    sftcount 0 count = count
--    sftcount mask' count = if (mask' .&. 1) == 1 then count else sftcount (mask' `shiftR` 1) (count + 1)

instance Class Data where
--  mkEmpty = (0,(0,0))
  header x = x
  transport_error_indicator    = (> 0) . (.&. 0x80) . fst -- 1 10000000 00000000 00000000 
  payload_unit_start_indicator = (> 0) . (.&. 0x40) . fst -- 1 01000000 00000000 00000000 
  transport_priority           = (> 0) . (.&. 0x20) . fst -- 1 00100000 00000000 00000000 
  pid                        x = let l = (`shiftL` 8) $ fromInteger $ toInteger $ (.&. 0x1F) $ fst x :: Word16 -- 13 00011111 11111111 00000000 
                                     r = fromInteger $ toInteger $ fst $ snd x :: Word16
                                 in l .&. r
  transport_scrambling_control_raw = (`shiftR` 6) . (.&. 0xC0) . snd . snd -- 2 00000000 00000000 11000000
  adaptation_field_control     = (`shiftR` 4) . (.&. 0x30) . snd . snd -- 2 00000000 00000000 00110000
  continuity_counter           = (.&. 0x0F) . snd . snd -- 4 00000000 00000000 00001111

instance EmptyExist Data where
  mkEmpty = (0,(0,0))

parse :: ByteString -> Data
parse = parseFromWord8 . unpack

parseFromWord8 :: [Word8] -> Data
parseFromWord8 [] = mkEmpty
parseFromWord8 (x:[]) = (x,(0,0))
parseFromWord8 (x:(y:[])) = (x,(y,0))
parseFromWord8 (x:(y:(z:zs))) = (x,(y,z))
