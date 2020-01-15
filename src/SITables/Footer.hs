{-# LANGUAGE FlexibleInstances #-}

module SITables.Footer where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(EmptyExist(..),BitsLen,BytesHolderIO(..))
import Parser(HasParser(..),FromWord64(..),ParseResult(..))
class Class a where
  footer :: a -> Data
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
  
-- data Symbol = CRC_32 deriving (Eq,Enum,Bounded)

-- update :: Symbol -> ValueCache -> Data -> (Data,Maybe Symbol)
-- update CRC_32 v st = (MkData $ fromValueCache v, Nothing)

-- result :: Data -> Maybe Data
-- result x = Just x

-- instance ParseConditionSymbol Symbol where
--   getLen CRC_32 = 32

_parseIOFlow :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = getBitsIO_M fh [
    (32 , (\(v,d) -> d { _crc_32 = fromWord64 v}))
    ] init
  
instance HasParser Data where
  parseIOFlow = flowStart |>>= _parseIOFlow

-- length :: BitsLen
-- length = bitsLength (allSymbols :: [Symbol])
