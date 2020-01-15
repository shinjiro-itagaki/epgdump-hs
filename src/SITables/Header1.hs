{-# LANGUAGE FlexibleInstances #-}

module SITables.Header1 where
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Common
import Common(EmptyExist(..),BitsLen,BytesHolderIO(..),BytesLen,BytesCounter(..))
import Parser(HasParser(..),FromWord64(..),ParseResult(..),mapParseResult,parseFlow)
import SITables.Common()

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
  }

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

_parseIOFlow :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = do
  (res,fh2) <- getBitsIO_M fh [
    (8 , (\(v,d) -> d { _table_id                 = fromWord64 v})),
    (1 , (\(v,d) -> d { _section_syntax_indicator = fromWord64 v})),
    (1 , (\(v,d) -> d { _reserved_future_use      = fromWord64 v})),
    (2 , (\(v,d) -> d { _reserved1                = fromWord64 v})),
    (12, (\(v,d) -> d { _section_length           = fromWord64 v}))
    ] init
  return (res, (resetBytesCounter fh2))

instance HasParser Data where
  parseIOFlow = flowStart |>>= _parseIOFlow

parseFlow :: (BytesHolderIO bh, HasParser a, Class a) => bh -> a -> IO (ParseResult a, bh)
parseFlow = Parser.parseFlow caster
  where
    caster :: (Class a) => Data -> a -> a
    caster header1 d = setHeader1 d header1
