module Descriptor.Header (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(EmptyExist(..),BitsLen,BytesLen,ByteString)
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M)
import qualified BytesReader.HolderIO as HolderIO
import qualified BytesReader.Counter as Counter
import qualified Parser
import FromWord64 hiding (Class)

class Class a where
  header            :: a -> Data
  setHeader         :: a -> Data -> a
  descriptor_tag    :: a -> Word8
  descriptor_tag = descriptor_tag . header
  descriptor_length :: a -> Word8
  descriptor_length = descriptor_length . header

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8
  }

instance Class Data where
  header x = x
  setHeader x y = y
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

_parseIOFlow :: (HolderIO.Class bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = do
  (res,fh2) <- getBitsIO_M fh [
    (8 , (\(v,d) -> d { _descriptor_tag    = fromWord64 v})),
    (8 , (\(v,d) -> d { _descriptor_length = fromWord64 v}))
    ] init
  return (res, (Counter.resetBytesCounter fh2))

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty

instance Parser.Class Data where
  parseIOFlow = flowStart |>>= _parseIOFlow

parseIO :: (HolderIO.Class bh) => bh -> IO (ParseResult Data, bh)
parseIO = Parser.parseIO
