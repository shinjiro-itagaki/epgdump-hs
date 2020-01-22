{-# LANGUAGE FlexibleInstances #-}

module SITables.Footer where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(EmptyExist(..),BitsLen,BytesLen,BitsLen)
import qualified BytesReader.Base as BytesReaderBase
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M)
import FromWord64 hiding (Class)
import qualified Parser

class (Show a) => Class a where
  footer :: a -> Data
  
  setFooter :: a -> Data -> a
  
  crc_32_bitslen :: a -> BitsLen
  crc_32_bitslen _ = 32

  crc_32_byteslen :: a -> BytesLen
  crc_32_byteslen = (`div` 8) . crc_32_bitslen

  crc_32 :: a -> Word32
  crc_32 = _crc_32 . footer
  
data Data = MkData {
  _crc_32 :: Word32
  } deriving (Show)

instance Class Data where
  setFooter x footer' = footer'
  footer  x = x
  crc_32    = _crc_32

instance EmptyExist Data where
  mkEmpty = MkData {
    _crc_32 = mkEmpty
    }

_parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = getBitsIO_M fh [
    (crc_32_bitslen init , (\(v,d) -> d { _crc_32 = fromWord64 v}))
    ] init
  
instance Parser.Class Data where
  parseIOFlow = flowStart |>>= _parseIOFlow

parseFlow :: (BytesReaderBase.Class bh, Parser.Class a, Class a) => bh -> a -> IO (ParseResult a, bh)
parseFlow = Parser.parseFlow caster
  where
    caster :: (Class a) => Data -> a -> a
    caster footer d = setFooter d footer
