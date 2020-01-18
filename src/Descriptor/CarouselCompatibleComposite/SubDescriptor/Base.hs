module Descriptor.CarouselCompatibleComposite.SubDescriptor.Base where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(EmptyExist(..),BitsLen,BytesHolderIO(..),BytesLen,BytesCounter(..),ByteString)
import Parser(HasParser(..),FromWord64(..),ParseResult(..),mapParseResult,parseFlow)

import Common(ByteString)
import qualified Descriptor.Header as Header

class (Header.Class a) => Class a where
