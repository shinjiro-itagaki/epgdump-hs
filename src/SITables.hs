{-# LANGUAGE FlexibleInstances #-}

module SITables(
  ) where
  
import SITables.Common(CommonHeader(..) ,CommonHeader2(..))
import SITables.BAT
import SITables.BIT
import SITables.Common
import SITables.EIT
import SITables.LDT
import SITables.NBIT
import SITables.NIT
import SITables.PCAT
import SITables.RST
import SITables.SDT
import SITables.ST
import SITables.TDT
import SITables.TOT

data Data =
  Other
  | None
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
  
