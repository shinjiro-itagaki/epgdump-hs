{-# LANGUAGE FlexibleInstances #-}

module Descriptor.Base where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(EmptyExist(..),BitsLen,BytesHolderIO(..),BytesLen,BytesCounter(..),ByteString)
import Parser(HasParser(..),FromWord64(..),ParseResult(..),mapParseResult,parseFlow)

class Class a where
  fromByteString :: ByteString -> (Maybe a,ByteString)



