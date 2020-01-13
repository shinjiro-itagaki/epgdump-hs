{-# LANGUAGE FlexibleInstances #-}

module SITables(
  ) where
  
import qualified SITables.BAT
import qualified SITables.BIT
import qualified SITables.Common
import qualified SITables.EIT
import qualified SITables.LDT
import qualified SITables.NBIT
import qualified SITables.NIT
import qualified SITables.PCAT
import qualified SITables.RST
import qualified SITables.SDT
import qualified SITables.ST
import qualified SITables.TDT
import qualified SITables.TOT

import Data.ByteString(ByteString)
import TS.Packet(FromPackets(..))
import Parser(HasParser(..))

instance FromPackets SITables.EIT.Data where
  pids      _ = SITables.EIT.pids
  table_ids _ = SITables.EIT.table_ids

data Data =
  Other
  | Null
  | BAT  SITables.BAT.Data
  | BIT  SITables.BIT.Data
  | EIT  SITables.EIT.Data
  | LDT  SITables.LDT.Data
  | NBIT SITables.NBIT.Data
  | NIT  SITables.NIT.Data
  | PCAT SITables.PCAT.Data
  | RST  SITables.RST.Data
  | SDT  SITables.SDT.Data
  | ST   SITables.ST.Data
  | TDT  SITables.TDT.Data
  | TOT  SITables.TOT.Data

parse :: (HasParser a) => [(a -> Data)] -> ByteString -> (Data,ByteString)
parse constructors bytes = (Null,bytes)
