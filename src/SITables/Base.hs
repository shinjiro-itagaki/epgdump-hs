{-# LANGUAGE FlexibleInstances #-}

module SITables.Base where
import Common(BytesLen,EmptyExist(..),PID,TableID,TableID,PID,PIDs(..),PID_And_TableID(..),Matcher(..))
import BytesReader(Holder(..),HolderIO(..))
import SITables.Common(SITableIDs(..),(==.=))
import qualified SITables.Header1 as Header1
import qualified SITables.Footer as Footer
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser
import qualified Parser.Result as Result

-- header1は必須
-- footerはないものもある
class (Header1.Class a, SITableIDs a, Parser.Class a) => Class a where
  footer                     :: a -> Maybe Footer.Data
  parseIOFlowAfterHeader1    :: (HolderIO bh) => ParseIOFlow bh a

  parseIO :: (HolderIO bh) => a -> Header1.Data -> bh -> IO (ParseResult a, bh)
  parseIO init header1 bh =
    if (Header1.table_id header1) =|== (table_ids init)
      then execParseIOFlow bh (Header1.setHeader1 init header1) parseIOFlowAfterHeader1
      else return (Result.NotMatch,bh)

  crc_length                 :: a -> BytesLen
  crc_length x = case footer x of
    Just footer -> Footer.crc_32_byteslen footer
    Nothing     -> 0
      
  section_length_without_crc :: a -> BytesLen
  section_length_without_crc x = (Header1.section_length x) - (crc_length x)
  
