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
  
parse :: ByteString -> (Data,ByteString)
parse bytes = (Null,bytes)
