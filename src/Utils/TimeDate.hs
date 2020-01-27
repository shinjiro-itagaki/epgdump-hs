module Utils.TimeDate(
  Class(..)
  ,Data
  ,mk
  ,Month(..)
  ,Weekday(..)
  ,Day(..)
  ,Hour(..)
  ,Min(..)
  ,Sec(..)
  ) where

import Common
import qualified Utils.ToNum as ToNum
import qualified Utils.FromNum as FromNum
import Data.Ratio(Rational,(%))
import qualified Utils.FromByteString as FromByteString
import Utils.FromByteString(fromByteString, fromByteStringWithRest)
import qualified Utils.EmptyExist as EmptyExist
import Utils.EmptyExist(mkEmpty)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Show,Eq,Enum,Bounded)

data Weekday = Mon | Tue | Wed | Tur | Fri | Sat | Sun
  deriving (Show,Eq,Ord,Enum,Bounded)

data Day = D01 | D02 | D03 | D04 | D05 | D06 | D07 | D08 | D09 | D10
         | D11 | D12 | D13 | D14 | D15 | D16 | D17 | D18 | D19 | D20
         | D21 | D22 | D23 | D24 | D25 | D26 | D27 | D28 | D29 | D30
         | D31
  deriving (Show,Eq,Ord,Enum,Bounded)

data Hour = H00 | H01 | H02 | H03 | H04 | H05 | H06 | H07 | H08 | H09
          | H10 | H11 | H12 | H13 | H14 | H15 | H16 | H17 | H18 | H19
          | H20 | H21 | H22 | H23
  deriving (Show,Eq,Ord,Enum,Bounded)

data Min = M00 | M01 | M02 | M03 | M04 | M05 | M06 | M07 | M08 | M09
         | M10 | M11 | M12 | M13 | M14 | M15 | M16 | M17 | M18 | M19
         | M20 | M21 | M22 | M23 | M24 | M25 | M26 | M27 | M28 | M29
         | M30 | M31 | M32 | M33 | M34 | M35 | M36 | M37 | M38 | M39
         | M40 | M41 | M42 | M43 | M44 | M45 | M46 | M47 | M48 | M49
         | M50 | M51 | M52 | M53 | M54 | M55 | M56 | M57 | M58 | M59
  deriving (Show,Eq,Ord,Enum,Bounded)

data Sec = S00 | S01 | S02 | S03 | S04 | S05 | S06 | S07 | S08 | S09
         | S10 | S11 | S12 | S13 | S14 | S15 | S16 | S17 | S18 | S19
         | S20 | S21 | S22 | S23 | S24 | S25 | S26 | S27 | S28 | S29
         | S30 | S31 | S32 | S33 | S34 | S35 | S36 | S37 | S38 | S39
         | S40 | S41 | S42 | S43 | S44 | S45 | S46 | S47 | S48 | S49
         | S50 | S51 | S52 | S53 | S54 | S55 | S56 | S57 | S58 | S59
  deriving (Show,Eq,Ord,Enum,Bounded)

class Class a where
  year  :: a -> Integer
  month :: a -> Month
  day   :: a -> Day
  hour  :: a -> Hour
  min   :: a -> Min
  sec   :: a -> Sec
  weekday :: a -> Weekday
  -- WY: Week number year since 1900
  wy :: (Num b) => a -> b
  -- WN: Week number in accordance with ISO 2015
  wn :: (Num b) => a -> b


data Data = MkData {
  _mjd :: Word16, -- 16
  _jst :: (Word8,Word8,Word8) -- 24
  } deriving (Eq,Ord,Show)

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let ((mjd,jst),rest) = fromByteStringWithRest bs
    in (MkData mjd jst, rest)

mk :: Word16 -> (Word8,Word8,Word8) -> Data
mk = MkData

instance ToNum.Class Month where
  to_num = fromInteger . toInteger . (+1) . fromEnum

instance ToNum.Class Weekday where
  to_num = fromInteger . toInteger . (+1) . fromEnum

instance ToNum.Class Day where
  to_num = fromInteger . toInteger . (+1) . fromEnum

instance ToNum.Class Hour where
  to_num = fromInteger . toInteger . fromEnum

instance ToNum.Class Min where
  to_num = fromInteger . toInteger . fromEnum

instance ToNum.Class Sec where
  to_num = fromInteger . toInteger . fromEnum

instance FromNum.Class Month where
  from_num = toEnum . fromInteger . pred . toInteger

instance FromNum.Class Weekday where
  from_num = toEnum . fromInteger . pred . toInteger

instance FromNum.Class Day where
  from_num = toEnum . fromInteger . pred . toInteger

instance FromNum.Class Hour where
  from_num = toEnum . fromInteger . toInteger

instance FromNum.Class Min where
  from_num = toEnum . fromInteger . toInteger

instance FromNum.Class Sec where
  from_num = toEnum . fromInteger . toInteger

-- mjd' :: Data -> Rational
-- mjd' = (% 1) . toInteger . _mjd
-- mjd' :: Data -> Double
-- mjd' = fromInteger . toInteger . _mjd


-- d365 = 1461 % 4 :: Rational
-- r306001 = 306001 % 10000 :: Rational
-- r149561 = 149561 % 10 :: Rational
-- r14956  = 14956  % 1 :: Rational
-- r150782 = 150782 % 10 :: Rational
-- rfloor  :: Rational -> Rational
-- rfloor  = (% 1) . floor 
-- r214464 = 214464 % 100 :: Rational
-- r79 = 79 % 10000 :: Rational
-- r1461_28 = 1461 % 28 :: Rational
-- r41 = 41 % 100 :: Rational

-- Y' = int[(MJD – 15078.2) / 365.25]
-- y' :: Data -> Rational
-- y' d = let _mjd' = mjd' d
--        in (% 1) $ floor $ ((_mjd' - r150782) / d365)
-- y' :: Data -> Double
-- y' d = let _mjd' = mjd' d
--        in floor $ ((_mjd' - 15078.2) / 365.25)

-- -- M' = int{[MJD – 14956.1 – int (Y' × 365.25)] / 30.6001}
-- m' :: Data -> Double
-- m' d = let _mjd' = mjd' d :: Double
--            _y'   = y' d :: Double
--            _mjd'2 = _mjd' – (14956.1 :: Double)
--            _y'2 = _y' * (365.25 :: Double)
--        in  (_mjd'2 – (_y'2)) / (30.6001 :: Double)

-- -- D = MJD – 14956 – int(Y' × 365.25) – int(M' × 30.6001)
-- d' :: Data -> Rational
-- d' d = let _mjd' = mjd' d
--            _y' = y' d
--            _m' = m' d
--        in _mjd' – r14956 – (rfloor (_y'*d365)) - (rfloor (_m' * r306001))

-- -- M' = 14 or M' = 15: K = 1
-- -- In other cases,: K = 0
-- k :: Data -> Rational
-- k d = let _m' = m' d
--        in if _m' == 14 || _m' == 15 then 1 else 0

-- -- W = int[(MJD / 7) – 2144.64]
-- w' :: Data -> Integer
-- w' d = let _mjd' = mjd' d
--        in floor $ (_mjd / 7) - r214464

-- instance Class Data where
--   -- Y: Year from 1900 (For example, 2003 is Y=103)
--   -- Y = Y' + K  
--   year d = let _y' = y' d
--                _k = k d
--            in (toRational 1900) + _y' + _k

--   -- M: Month (January = 1 to December = 12)
--   -- M = M' – 1 – K × 12  
--   month d = let _m' = m' d
--                 _k = k d
--                 _m = _m' – 1 – (_k * 12)
--             in from_num _m

--   -- D: Date (1 to 31)
--   -- MJD – 14956 – int(Y' × 365.25) – int(M' × 30.6001)  
--   day d = let _mjd' = mjd' d
--               _y' = y' d
--               _m' = m' d -- :: a -> Day
--               _d = _mjd' – (toRational 14956) – (rfloor $ _y'*d365) – (rfloor $ m'*r306001)
--           in from_num _d
--   hour = from_num . (`shiftR` 16) . (.&. 0xFF0000) . _jst
--   min  = from_num . (`shiftR`  8) . (.&. 0x00FF00) . _jst
--   sec  = from_num .                 (.&. 0x0000FF) . _jst
  
--   -- WD: Week day (Monday = 1 to Sunday = 7)
--   -- WD = [(MJD + 2) mod 7] + 1  
--   weekday d = let wd' = (+1) $ (`mod` 7) $ (mjd' d) + 2
--               in from_num wd'

--   -- WY = int[(W × 28 / 1461) – 0.0079]
--   wy d = floor $ ((w' d) * (28 % 1461)) -  r79

--   -- WN = W – int[(WY × 1461 / 28) + 0.41]
--   wn d = ((w' d) % 1) - (rfloor $ ((((wy d) % 1) * (r1461_28)) + r41))
    
-- Used symbol:
-- MJD: Modified Julian Date (Japan time)
-- JTC: Japan Time Code
-- b) Method to find MJD from year, month and date (Y, M, D)
-- Where in case of M = 1 or M = 2: L = 1
-- In other cases: L = 0
-- MJD = 14956 + D + int[(Y – L) × 365.25] + int[(M + 1 + L × 12) × 30.6001]
-- c) Method to find week day (WD) from MJD
-- d) Method to find MJD from WY, WN and WD
-- MJD = 15012 + WD + 7 × {WN + int[(WY × 1461 / 28) + 0.41]}
-- e) Method to find WY and WN from MJD


-- Example: MJD = 45218 W = 4315
-- Y = (19)82 WY = (19)82
-- M = 9 (Sept.) WN = 36
-- D = 6 WD = 1 (Monday)
-- [Note]: These formulas are effective from March 1, 1900 to February 28, 2100.
