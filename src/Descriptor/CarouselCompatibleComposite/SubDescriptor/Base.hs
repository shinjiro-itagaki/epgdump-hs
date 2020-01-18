module Descriptor.CarouselCompatibleComposite.SubDescriptor.Base where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(EmptyExist(..),BitsLen,BytesLen,ByteString)
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M)
import qualified Parser

import Common(ByteString)
import qualified Descriptor.Header as Header

class (Header.Class a) => Class a where
