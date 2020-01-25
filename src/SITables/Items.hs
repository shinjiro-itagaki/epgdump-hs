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
import qualified BytesReader.Base as BytesReaderBase
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser
import qualified Parser.Result as Result
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty,snoc)
import Data.Maybe(fromMaybe)
import qualified BytesReader.Counter as Counter


class (Parser.Class a, Show a) => Element a where
  gather :: (BytesReaderBase.Class bh, Parser.Class b) => (b -> a -> b) -> BytesLen -> bh -> b -> IO (ParseResult b, bh)
  gather appender restlen fh init
      | restlen < 1 = return (Result.Parsed init, fh)
      | otherwise = do
          let old_count = Counter.getBytesCounter fh
          -- putStrLn "Items::gather"
          -- putStrLn $ ("old counter = " ++ ) $ show old_count
          -- putStrLn $ "Items::gather restlen = " ++ show restlen
          res@(res_item,fh') <- parseIO fh
          case res_item of
            Result.Parsed item -> do
              -- putStrLn $ ("new counter = " ++)  $ show $ Counter.getBytesCounter fh'
              let new_count = Counter.getBytesCounter fh'
                  diff_count = new_count - old_count
                in
                if diff_count > 0 && restlen > diff_count
                then gather appender (restlen - diff_count) fh' (appender init item) -- return (Result.DataIsTooShort $ Just restlen, fh')
                else return (Result.Parsed $ appender init item, fh')
            _ -> return $ (\x -> (x,fh')) $ mapParseResult (\_ -> init) res_item
