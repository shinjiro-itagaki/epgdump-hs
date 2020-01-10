{-# LANGUAGE FlexibleInstances #-}

module SITables.Header2 where
import Data.Word(Word64, Word32, Word16, Word8)
import Descriptor
import Common(EmptyExist(..))
import Parser(ParseConditionSymbol(..),FromValueCache(..),ValueCache)

class Class a where
  reserved2                   :: a -> Word8
  version_number              :: a -> Word8
  current_next_indicator      :: a -> Bool
  section_number              :: a -> Word8
  last_section_number         :: a -> Word8
