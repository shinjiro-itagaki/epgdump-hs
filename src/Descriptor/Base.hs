{-# LANGUAGE FlexibleInstances #-}

module Descriptor.Base where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(EmptyExist(..),BitsLen,BytesLen,ByteString)
import BytesReader.Counter
import qualified Descriptor.Header as Header
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
-- import Parser(FromWord64(..),ParseResult(..),mapParseResult,parseFlow)
import qualified BytesReader
import qualified BytesReader.Base as BytesReaderBase
import qualified Data.ByteString.Lazy as BS
import Data.Bits(shiftL,(.|.))
-- import qualified Utils.FromByteString as FromByteString

class (Show a, Header.Class a) => Class a where
  fromByteString :: ByteString -> (Maybe a,ByteString)
  fromByteString bs =
    let mx = BS.uncons bs >>= (\(x,rest') -> BS.uncons rest' >>= (\(y,rest'') -> Just (Header.mk x y,rest'')))
    in case mx of
         Just (header,rest) -> fromByteStringAfterHeader header rest
         Nothing -> (Nothing,BS.empty)
  
  fromByteStringAfterHeader :: Header.Data -> ByteString -> (Maybe a,ByteString)
  
gather :: BytesLen -> (ByteString -> (Maybe b, ByteString)) -> (a -> b -> a) -> ByteString -> a -> (a,ByteString)
gather alllen maker appender bs container
  | BS.null bs = (container, bs)
  | otherwise = 
      let len = BS.length bs
          (m_elem,rest) = maker bs
          restlen = BS.length rest
          difflen = fromInteger $ toInteger $ (len - restlen) 
          newalllen = alllen - difflen
          do_next   = alllen > difflen
      in case m_elem of
           Just e  -> let newcontainer = (appender container e)
                      in if do_next
                         then gather newalllen maker appender rest newcontainer
                         else (newcontainer,rest)
           Nothing -> (container,rest)
           

gatherByteString :: BytesLen -> (ByteString -> a) -> ByteString -> (Maybe a,ByteString)
gatherByteString len f bs =
  let len' = toInteger len
      (bs2,rest) = BS.splitAt (fromInteger len') bs
      d = f bs2
      rtn = if len' > (toInteger $ BS.length bs2) then Nothing else Just d
  in (rtn, rest)

gatherWord16 :: BytesLen -> (a -> Word16 -> a) -> a -> ByteString -> (Maybe a, ByteString)
gatherWord16 len appender init bs =
  let len' = toInteger len
      (bs2,rest) = BS.splitAt (fromInteger len') bs
      word8s = BS.unpack bs2
      xs = [fromInteger $ toInteger x | x <- word8s, x `mod` 2 == 0]
      ys = [fromInteger $ toInteger x | x <- word8s, x `mod` 2 == 1]
      word16s = Prelude.map (\(x,y) -> (x `shiftL` 8) .|. y) $ Prelude.zip xs ys
      d = Prelude.foldl appender init word16s
      rtn = if len' > (toInteger $ BS.length bs2) then Nothing else Just d
  in (rtn, rest)

