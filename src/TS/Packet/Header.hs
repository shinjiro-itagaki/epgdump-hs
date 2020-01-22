{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.Header where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import Data.ByteString.Lazy(unpack)
import Common(ByteString,EmptyExist(..),PID,TableID)

data ScrambleControl =
  NoScramble -- '00'
  | UndefScramble -- '01' 
  | EvenKey -- '10'
  | OddKey -- '11'
  deriving (Show)

data ContinuityCounter = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | C11 | C12 | C13 | C14 | C15 deriving (Enum,Eq,Ord,Show)

next :: ContinuityCounter -> ContinuityCounter
next C15 = C0
next x = succ x

toContinuityCounter :: (Num i, Bits i) => i -> ContinuityCounter
toContinuityCounter i = 
  case i of
      0         -> C0
      1         -> C1
      2         -> C2
      3         -> C3
      4         -> C4
      5         -> C5
      6         -> C6
      7         -> C7
      8         -> C8
      9         -> C9
      10        -> C10
      11        -> C11
      12        -> C12
      13        -> C13
      14        -> C14
      15        -> C15
      otherwise -> toContinuityCounter $ i .&. 0x0F



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
  
  pid                          :: a -> PID -- 13 bits
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
  
  continuity_counter :: a -> ContinuityCounter -- 4 bits
  continuity_counter = continuity_counter . header

  continuity_ok :: a -> Maybe ContinuityCounter -> Bool
  continuity_ok x Nothing  = True
  continuity_ok x (Just c) = (next c) == continuity_counter x

  -- '10' or '11'
  has_adaptation_field         :: a -> Bool
  has_adaptation_field         = (> 0) . (.&. 0x02) . adaptation_field_control

  -- '01' or '11'
  has_payload                  :: a -> Bool
  has_payload                  = (> 0) . (.&. 0x01) . adaptation_field_control

  transport_scrambling_control_raw :: a -> Word8
  transport_scrambling_control_raw = transport_scrambling_control_raw . header
  
                                   
type Data = Word32

instance Class Data where
  header x = x
  transport_error_indicator    = (> 0) . (.&. 0x800000) -- 1 10000000 00000000 00000000 
  payload_unit_start_indicator = (> 0) . (.&. 0x400000) -- 1 01000000 00000000 00000000 
  transport_priority           = (> 0) . (.&. 0x200000) -- 1 00100000 00000000 00000000 
  pid                              = fromInteger . toInteger . (`shiftR` 8) . (.&. 0x1FFF00) -- 13 00011111 11111111 00000000
  transport_scrambling_control_raw = fromInteger . toInteger . (`shiftR` 6) . (.&. 0x0000C0) --  2 00000000 00000000 11000000
  adaptation_field_control         = fromInteger . toInteger . (`shiftR` 4) . (.&. 0x000030) --  2 00000000 00000000 00110000
  continuity_counter               = toContinuityCounter                    . (.&. 0x00000F) --  4 00000000 00000000 00001111

parse :: ByteString -> Data
parse = parseFromWord8 . unpack

parseFromWord8 :: [Word8] -> Data
parseFromWord8 []             = mkEmpty
parseFromWord8 (x:[])         = ((`shiftL` 16) $ ((fromInteger $ toInteger x) :: Word32))
parseFromWord8 (x:(y:[]))     = ((`shiftL` 16) $ ((fromInteger $ toInteger x) :: Word32)) .|. ((`shiftL` 8) $ ((fromInteger $ toInteger y) :: Word32))
parseFromWord8 (x:(y:(z:zs))) = ((`shiftL` 16) $ ((fromInteger $ toInteger x) :: Word32)) .|. ((`shiftL` 8) $ ((fromInteger $ toInteger y) :: Word32)) .|. ((fromInteger $ toInteger z) :: Word32)
-- parseFromWord8 (x:(y:(z:zs))) = 1
