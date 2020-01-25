module Descriptor.Header (
  Class(..)
  ,Data
  ,mk
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(EmptyExist(..),BitsLen,BytesLen,ByteString)
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M)
import qualified BytesReader.Base as BytesReaderBase
import qualified BytesReader.Counter as Counter
import qualified Parser
import FromWord64 hiding (Class)

class (Show a) => Class a where
  header            :: a -> Data
  descriptor_tag    :: a -> Word8
  descriptor_tag = descriptor_tag . header
  descriptor_length :: (Num b) => a -> b
  descriptor_length = fromInteger . toInteger . descriptor_length . header

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8
  } deriving (Show)

mk :: Word8 -> Word8 -> Data
mk x y = MkData {
  _descriptor_tag    = x,
  _descriptor_length = y
  }

instance Class Data where
  header x = x

-- _parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (ParseResult Data, bh)
-- _parseIOFlow fh init = do
--   (res,fh2) <- getBitsIO_M fh [
--     (8 , (\(v,d) -> d { _descriptor_tag    = fromWord64 v})),
--     (8 , (\(v,d) -> d { _descriptor_length = fromWord64 v}))
--     ] init
--   return (res, (Counter.resetBytesCounter fh2))

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty

-- instance Parser.Class Data where
--   parseIOFlow = flowStart |>>= _parseIOFlow

-- parseIO :: (BytesReaderBase.Class bh) => bh -> IO (ParseResult Data, bh)
-- parseIO = Parser.parseIO
