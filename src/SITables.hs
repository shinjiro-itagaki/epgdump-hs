{-# LANGUAGE FlexibleInstances #-}

module SITables where
import qualified SITables.Base as Base

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
import qualified TS.Packet as Packet
import TS.Packet(FromPackets(..))
import Parser(ParseResult(..),mapParseResult)
import qualified BytesReader.HolderIO as HolderIO
import qualified SITables.Header1 as Header1
import Common(EmptyExist(..))
import qualified Parser.Result as Result

data Data =
  Other
  | Null
  | IsBAT  SITables.BAT.Data
  | IsBIT  SITables.BIT.Data
  | IsEIT  SITables.EIT.Data
  | IsLDT  SITables.LDT.Data
  | IsNBIT SITables.NBIT.Data
  | IsNIT  SITables.NIT.Data
  | IsPCAT SITables.PCAT.Data
  | IsRST  SITables.RST.Data
  | IsSDT  SITables.SDT.Data
  | IsST   SITables.ST.Data
  | IsTDT  SITables.TDT.Data
  | IsTOT  SITables.TOT.Data

data Constructor =
  ConsBAT (SITables.BAT.Data -> Data)
  | ConsBIT  (SITables.BIT.Data -> Data)
  | ConsEIT  (SITables.EIT.Data -> Data)
  | ConsLDT  (SITables.LDT.Data -> Data)
  | ConsNBIT (SITables.NBIT.Data -> Data)
  | ConsNIT  (SITables.NIT.Data -> Data)
  | ConsPCAT (SITables.PCAT.Data -> Data)
  | ConsRST  (SITables.RST.Data -> Data)
  | ConsSDT  (SITables.SDT.Data -> Data)
  | ConsST   (SITables.ST.Data -> Data)
  | ConsTDT  (SITables.TDT.Data -> Data)
  | ConsTOT  (SITables.TOT.Data -> Data)

cBAT = ConsBAT IsBAT
cBIT = ConsBIT IsBIT
cEIT = ConsEIT IsEIT
cLDT = ConsLDT IsLDT
cNBIT = ConsNBIT IsNBIT
cNIT = ConsNIT IsNIT
cPCAT = ConsPCAT IsPCAT
cRST = ConsRST IsRST
cSDT = ConsSDT IsSDT
cST  = ConsST IsST
cTDT = ConsTDT IsTDT
cTOO = ConsTOT IsTOT

parseIO :: (HolderIO.Class bh) => bh -> state -> [Constructor] -> (Data -> state -> IO state) -> IO (ParseResult bh,state)
parseIO bh state conss hook = do
  (res_header1,bh2) <- Header1.parseIO bh
  return (Result.Parsed bh,state)

_parseIO :: (HolderIO.Class bh) => bh -> Constructor -> Header1.Data -> IO (ParseResult Data, bh)
_parseIO bh cons header1 =
  case cons of
    ConsBAT  f -> impl' f
    ConsBIT  f -> impl' f
    ConsEIT  f -> impl' f
    ConsLDT  f -> impl' f
    ConsNBIT f -> impl' f
    ConsNIT  f -> impl' f
    ConsPCAT f -> impl' f
    ConsRST  f -> impl' f
    ConsSDT  f -> impl' f
    ConsST   f -> impl' f
    ConsTDT  f -> impl' f
    ConsTOT  f -> impl' f
  where
    impl' f = Base.parseIO mkEmpty header1 bh >>= (\(x,y) -> return (mapParseResult f x, y))
