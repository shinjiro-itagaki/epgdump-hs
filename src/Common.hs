{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Common where
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString(ByteString,pack,unpack,null,uncons)
import Data.Bits((.&.),shiftL,shiftR)

class HasOriginalNetworkID a where
  original_network_id :: a -> Word16

data ParseResult a = Parsed a ByteString | NotMatch | DataIsTooShort Word64

bitlenmax :: Word8
bitlenmax = 64

type BitLen = Word8

class Init a where
  init :: a
  empty :: a
  empty = Common.init

type ByteHeadValue = Word8
type ByteHeadRestBitCount = Word8
type ByteHead = (ByteHeadValue, ByteHeadRestBitCount)
type ValueCache = Word64

instance Init ByteHead where
  init = (0,0)

instance Init Word64 where
  init = 0

_readValue :: (FromWord64 a) => ValueCache -> BitLen -> (ByteHead,ByteString) -> (a,(ByteHead,ByteString))
_readValue curr   0                x = (fromWord64 curr, x) -- キャッシュしていた値を期待する値に変換して正常終了

 -- 先頭部分のWord8の読み取りが終了した場合
_readValue curr len ((head,0),bytes) =
  case Data.ByteString.uncons bytes of
    -- 残りのデータが空の場合
    Nothing              -> _readValue curr (len-1) ((0,8),bytes) -- バイト配列はもう終わりなのに、まだ読み取らないといけない場合、残りは全部0であったとみなして処理を続行
    -- 残りのデータがある場合
    Just (head2, bytes2) -> _readValue curr len ((head2,8),bytes2) -- まだバイト配列にWord8が残っているので、先頭部分のWord8を入れ替えて続行

-- まだ必要な分が終わっておらず、かつ先頭部分のWord8が読み取りが終わっていない場合
_readValue curr len ((head,rest),bytes) =
  let shiftCnt = (min len rest) :: Word8  -- shiftする数は残りの読み取るべき数と先頭部分のWord8の残りとのどちらか少ない方に合わせる
      shiftCntInt = (fromInteger $ toInteger shiftCnt) :: Int 
      plus = ((fromInteger . toInteger) $ (head `shiftR` (8 - shiftCntInt))) :: Word64 -- -- 例えば3ビット分を転写する場合、右に5ビット移動させることで左3ビットが示す値を作成する
      newcurr = (curr `shiftL` shiftCntInt) + plus -- 例えば3ビット分を転写する場合、currを左に3ビット移動させて、空いた右3ビットにheadの左3ビットを転写する
      newhead = (head `shiftL` shiftCntInt)
      newrest = rest - shiftCnt
      newlen  = len  - shiftCnt
  in _readValue newcurr newlen ((newhead,newrest),bytes)  -- 先頭のWord8の左側のビット値を読み取ってWord64に転写する
                 

class FromWord64 a where
  fromWord64 :: Word64 -> a
  readValue :: BitLen -> (ByteHead,ByteString) -> (a,(ByteHead,ByteString))
  readValue bitlen x
    | bitlen > bitlenmax = readValue bitlenmax x -- if bitlen is greater than bitlenmax, bitlen is treated as bitlenmax
    | bitlen == 0 = (fromWord64 0,x) -- if bitlen is 0, do nothing
    | otherwise = _readValue empty bitlen x

instance FromWord64 Bool where
  fromWord64 = (> 0) . (.&. 0x01)

class HasParser a where
  parse :: ByteString -> ParseResult a
--  parseInfo :: 

-- uimsbf: unsigned integer most significant bit first
-- bslbf: bit string,left bit first
-- remainder polynomial coefficients
data BitsType = Uimsbf | Bslbf | Rpchof
