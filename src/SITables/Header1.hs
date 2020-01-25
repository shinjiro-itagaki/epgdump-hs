{-# LANGUAGE FlexibleInstances #-}

module SITables.Header1 where
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Common
import Common(EmptyExist(..),BitsLen,BytesLen)
import qualified BytesReader.Base as BytesReaderBase
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO)
import FromWord64 hiding (Class)
import qualified Parser
import SITables.Common
import qualified BytesReader.Counter as Counter
import Data.Bits((.&.))

class Class a where
  header1                     :: a -> Data
  setHeader1                  :: a -> Data -> a

  table_id                 = table_id                 . header1  
  table_id                    :: a -> Common.TableID
  
  section_syntax_indicator    :: a -> Bool
  section_syntax_indicator = section_syntax_indicator . header1

  reserved_future_use         :: a -> Bool
  reserved_future_use      = reserved_future_use      . header1
  
  reserved1                   :: a -> Word8
  reserved1                = reserved1                . header1
  
  section_length              :: a -> BytesLen
  section_length           = section_length           . header1

data Data = MkData {
  _table_id                 :: Common.TableID,
  _section_syntax_indicator :: Bool,
  _reserved_future_use      :: Bool,
  _reserved1                :: Word8,
  _section_length           :: BytesLen
  } deriving (Show)

instance Class Data where
  header1                x = x
  setHeader1  self header1 = header1
  table_id                 = _table_id 
  section_syntax_indicator = _section_syntax_indicator
  reserved_future_use      = _reserved_future_use
  reserved1                = _reserved1
  section_length           = _section_length

instance EmptyExist Data where
  mkEmpty = MkData {
    _table_id                 = mkEmpty,
    _section_syntax_indicator = mkEmpty,
    _reserved_future_use      = mkEmpty,
    _reserved1                = mkEmpty,
    _section_length           = mkEmpty
    }

_parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = do
  -- putStrLn "Header1::_parseIOFlow"
  (res,fh2) <- getBitsIO_M fh [
    (8 , (\(v,d) -> d { _table_id                 = fromWord64 v})),
    (1 , (\(v,d) -> d { _section_syntax_indicator = fromWord64 v})),
    (1 , (\(v,d) -> d { _reserved_future_use      = fromWord64 v})),
    (2 , (\(v,d) -> d { _reserved1                = fromWord64 v})),
    (12, (\(v,d) -> d { _section_length           = (0x3FF .&.) $ fromWord64 v}))
    ] init
  -- putStrLn $ show res
  return (res, (Counter.resetBytesCounter fh2))

instance Parser.Class Data where
  parseIOFlow = flowStart |>>= _parseIOFlow

parseFlow :: (BytesReaderBase.Class bh, Parser.Class a, Class a) => bh -> a -> IO (ParseResult a, bh)
parseFlow = Parser.parseFlow caster
  where
    caster :: (Class a) => Data -> a -> a
    caster header1 d = setHeader1 d header1

parseIO :: (BytesReaderBase.Class bh) => bh -> IO (ParseResult Data, bh)
parseIO = Parser.parseIO
