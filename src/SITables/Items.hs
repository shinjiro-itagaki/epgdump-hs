{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.Items(
  Element(..)
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import qualified SITables.Base as Base
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import Common(EmptyExist(..),PID,TableID,BytesLen)
import qualified BytesReader.HolderIO as HolderIO
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser
import qualified Parser.Result as Result
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty,snoc)
import Data.Maybe(fromMaybe)
import qualified BytesReader.Counter as Counter

class (Parser.Class a) => Element a where
  gather :: (HolderIO.Class bh, Parser.Class b) => (b -> a -> b) -> BytesLen -> bh -> b -> IO (ParseResult b, bh)
  gather appender restlen fh init
      | restlen < 1 = return (Result.Parsed init, fh)
      | otherwise = do
          res@(res_item,fh') <- parseIO fh
          case res_item of
            Result.Parsed item -> gather appender (restlen - ((Counter.getBytesCounter fh') - (Counter.getBytesCounter fh))) fh' (appender init item)
            _           -> return $ (\x -> (x,fh')) $ mapParseResult (\_ -> init) res_item
