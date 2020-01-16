{-# LANGUAGE FlexibleInstances #-}

module SITables.Base where
import Common(BytesLen,HasOriginalNetworkID(..),EmptyExist(..),PID,TableID,BytesHolderIO(..),TableID,PID,PIDs(..))
import SITables.Common(SITableIDs(..))
import Parser(HasParser(..),FromWord64(..),ParseResult(..),ParseIOFlow(..),(>>==))
import qualified SITables.Header1 as Header1
import qualified SITables.Footer as Footer

-- header1は必須
-- footerはないものもある
class (Header1.Class a, SITableIDs a, HasParser a) => Class a where
  header1                    :: a -> Header1.Data
  footer                     :: a -> Maybe Footer.Data

  parseIOFlowAfterHeader1    :: (BytesHolderIO bh) => ParseIOFlow bh a

  parseIOFlow :: (BytesHolderIO bh) => ParseIOFlow bh a
  parseIOFlow =
    flowStart
    |>>= Header1.parseFlow
    >>== parseIOFlowAfterHeader1
  
  crc_length                 :: a -> BytesLen
  crc_length x = case footer x of
    Just footer -> Footer.crc_32_byteslen footer
    Nothing     -> 0
      
  section_length_without_crc :: a -> BytesLen
  section_length_without_crc x = (Header1.section_length x) - (crc_length x)
  

