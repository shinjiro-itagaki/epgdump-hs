{-# LANGUAGE FlexibleInstances #-}

module SITables.Footer where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(EmptyExist(..),BitsLen,BytesHolderIO(..))
import Parser(HasParser(..),FromWord64(..),ParseResult(..),parseFlow)
class Class a where
  footer :: a -> Data
  setFooter :: a -> Data -> a
  
  crc_32 :: a -> Word32
  crc_32 = _crc_32 . footer
  
data Data = MkData {
  _crc_32 :: Word32
  }

instance Class Data where
  footer  x = x
  crc_32    = _crc_32

instance EmptyExist Data where
  mkEmpty = MkData {
    _crc_32 = mkEmpty
    }

_parseIOFlow :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = getBitsIO_M fh [
    (32 , (\(v,d) -> d { _crc_32 = fromWord64 v}))
    ] init
  
instance HasParser Data where
  parseIOFlow = flowStart |>>= _parseIOFlow

parseFlow :: (BytesHolderIO bh, HasParser a, Class a) => bh -> a -> IO (ParseResult a, bh)
parseFlow = Parser.parseFlow caster
  where
    caster :: (Class a) => Data -> a -> a
    caster footer d = setFooter d footer
