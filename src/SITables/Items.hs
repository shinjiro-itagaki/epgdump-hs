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
import Common(EmptyExist(..),PID,TableID,BytesHolderIO(..),BytesLen,BytesCounter(getBytesCounter))
import Parser(FromWord64(..),ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import qualified Parser
import qualified Parser
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty,snoc)
import Data.Maybe(fromMaybe)

class (Parser.Class a) => Element a where
  gather :: (BytesHolderIO bh, Parser.Class b) => (b -> a -> b) -> BytesLen -> bh -> b -> IO (ParseResult b, bh)
  gather appender restlen fh init
      | restlen < 1 = return (Parsed init, fh)
      | otherwise = do
          res@(res_item,fh') <- parseIO fh
          case res_item of
            Parsed item -> gather appender (restlen - ((getBytesCounter fh') - (getBytesCounter fh))) fh' (appender init item)
            _           -> return $ (\x -> (x,fh')) $ mapParseResult (\_ -> init) res_item
