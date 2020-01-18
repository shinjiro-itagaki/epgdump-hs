{-# LANGUAGE FlexibleInstances #-}

module SITables.Header2 where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(EmptyExist(..),BitsLen,BytesHolderIO(..))
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M)
import FromWord64 hiding (Class)
import qualified Parser

class Class a where
  header2                :: a -> Data
  setHeader2             :: a -> Data -> a
  
  reserved2              :: a -> Word8
  reserved2              = reserved2              . header2
  
  version_number         :: a -> Word8
  version_number         = version_number         . header2
  
  current_next_indicator :: a -> Bool
  current_next_indicator = current_next_indicator . header2
  
  section_number         :: a -> Word8
  section_number         = section_number         . header2
  
  last_section_number    :: a -> Word8
  last_section_number    = last_section_number    . header2

data Data = MkData {
  _reserved2              :: Word8,
  _version_number         :: Word8,
  _current_next_indicator :: Bool,
  _section_number         :: Word8,
  _last_section_number    :: Word8
  }

instance Class Data where
  header2              x = x
  setHeader2         x y = y
  reserved2              = _reserved2
  version_number         = _version_number 
  current_next_indicator = _current_next_indicator
  section_number         = _section_number
  last_section_number    = _last_section_number


instance EmptyExist Data where
  mkEmpty = MkData {
    _reserved2                = mkEmpty,
    _version_number           = mkEmpty,
    _current_next_indicator   = mkEmpty,
    _section_number           = mkEmpty,
    _last_section_number      = mkEmpty
    }

_parseIOFlow :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = getBitsIO_M fh [
    (2 , (\(v,d) -> d { _reserved2              = fromWord64 v})),
    (5 , (\(v,d) -> d { _version_number         = fromWord64 v})),
    (1 , (\(v,d) -> d { _current_next_indicator = fromWord64 v})),
    (8 , (\(v,d) -> d { _section_number         = fromWord64 v})),
    (8 , (\(v,d) -> d { _last_section_number    = fromWord64 v}))
    ] init

instance Parser.Class Data where
  parseIOFlow = flowStart |>>= _parseIOFlow  

parseFlow :: (BytesHolderIO bh, Parser.Class a, Class a) => bh -> a -> IO (ParseResult a, bh)
parseFlow = Parser.parseFlow caster
  where
    caster :: (Class a) => Data -> a -> a
    caster header2 d = setHeader2 d header2
