{-# LANGUAGE FlexibleInstances #-}

module SITables.Header2 where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(EmptyExist(..),BitsLen)
import Parser(HasParser(..),ParseConditionSymbol(..),FromValueCache(..),ValueCache)
class Class a where
  header2                :: a -> Data
  
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
  
data Symbol = Reserved2 | VersionNumber | CurrentNextIndicator | SectionNumber | LastSectionNumber deriving (Eq,Enum,Bounded)

update :: Symbol -> ValueCache -> Data -> (Data,Maybe Symbol)
update Reserved2            v st = (st { _reserved2              = fromValueCache v },Nothing)
update VersionNumber        v st = (st { _version_number         = fromValueCache v },Nothing)
update CurrentNextIndicator v st = (st { _current_next_indicator = fromValueCache v },Nothing)
update SectionNumber        v st = (st { _section_number         = fromValueCache v },Nothing)
update LastSectionNumber    v st = (st { _last_section_number    = fromValueCache v },Nothing)

result :: Data -> Maybe Data
result x = Just x

instance ParseConditionSymbol Symbol where
  getLen Reserved2            = 2
  getLen VersionNumber        = 5 
  getLen CurrentNextIndicator = 1
  getLen SectionNumber        = 8
  getLen LastSectionNumber    = 8

instance HasParser Data where
  parse = startParse update result

length :: BitsLen
length = bitsLength (allSymbols :: [Symbol])
