{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BytesReader(
  Class(..)
  ,Data
  ,new
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(pack,unpack,null,uncons,empty)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import Data.Char(chr)
import Common(ByteString,BytesLen)
import Data.ByteString.Lazy(head,last,unpack)
import Data.Vector(Vector,empty,snoc,toList)
import qualified Data.ByteString.Lazy as BS
import qualified BytesReader.Status as Status
import qualified BytesReader.Handler as Handler
import qualified BytesReader.Counter as Counter
import qualified BytesReader.Base as Base
import qualified BytesReader.StockedBitsLen as StockedBitsLen
import qualified BytesReader.Base as BytesReaderBase

-- ロード済みの在庫Word8のうち何ビットが残っているかを示すフラグ
-- 在庫の消費は左側ビットが優先

class (Base.Class a, Status.Class a) => Class a where
  syncIO :: a -> Word8 ->IO a
  syncIO fh syncByte
    | (Base.loaded fh) == syncByte = return fh
    | otherwise = do
        isEOF' <- Base.isEOF $ fh
        if isEOF'
          then return fh
          else
          do
            (bytes,fh2) <- Base.getBytesIO fh 1
            x <- return $ Data.ByteString.Lazy.head bytes
            if x == syncByte
              then do
                return fh2
              else syncIO fh2 syncByte

data (Handler.Class h) => Data h = MkData {
  _handle         :: h,
  _pos            :: Word64, -- bits
  _size           :: BytesLen,
  _loaded         :: Word8,
  _bytesCounter   :: BytesLen
  }

new :: (Handler.Class h) => h -> IO (Data h)
new h = do
  size <- Handler.size h
  return MkData {
    _handle         = h,
    _pos            = 0,
    _size           = size,
    _loaded         = 0,
    _bytesCounter   = 0
    }


instance (Handler.Class h) => Status.Class (Data h) where
  pos = _pos
  size = _size
  
instance (Handler.Class h) => Counter.Class (Data h) where
  getBytesCounter     = _bytesCounter 
  resetBytesCounter x = x {_bytesCounter = 0 }

-- private method
_addPos :: (Handler.Class h) => (Data h) -> Word64 -> (Data h)
_addPos x i = x { _pos = i + (_pos x),  _bytesCounter = i + (_bytesCounter x) }

instance (Handler.Class h) => Base.Class (Data h) where
  isEOF = Handler.isEOF . _handle  

--  getBytesIO :: (Integral i) => a -> i -> IO (ByteString, a)
  getBytesIO x i
    | i < 1 = return (BS.empty, x)
    | otherwise = 
      let i1 = (fromInteger $ toInteger i) :: Int
      in Handler.hGet (_handle x) i1
         >>= (\bytes -> return $
                        if BS.null bytes
                        then (BS.empty, x)
                        else (bytes, _addPos (x {_loaded = BS.last bytes
                                                }) (fromInteger $ toInteger $ BS.length bytes)))
  
instance (Handler.Class h) => Class (Data h) where
