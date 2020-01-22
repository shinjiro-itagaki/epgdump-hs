{-# LANGUAGE FlexibleInstances #-}

module Descriptor.Base where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(EmptyExist(..),BitsLen,BytesLen,ByteString)
import BytesReader.HolderIO
import BytesReader.Counter
-- import Parser(FromWord64(..),ParseResult(..),mapParseResult,parseFlow)

class (Show a) => Class a where
  fromByteString :: ByteString -> (Maybe a,ByteString)
