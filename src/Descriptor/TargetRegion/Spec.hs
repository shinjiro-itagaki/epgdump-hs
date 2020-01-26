{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Descriptor.TargetRegion.Spec (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
--import qualified Descriptor.Base as Base
--import qualified Descriptor.Header as Header
import Data.Bits(testBit)
import qualified Utils.FromWord64 as FromWord64
import Utils.FromWord64 hiding (Class)

data TargetArea =
  EastHokkaido | WestHokkaido | Aomori | Iwate | Miyagi | Akita | Yamagata | Fukushima
  | Ibaraki | Tochigi | Gunma | Saitama | Chiba | Tokyo {- (excluding island (south west islands area) -} | Kanagawa
  | Niigata | Toyama | Ishikawa | Fukui
  | Yamanashi | Nagano
  | Gifu | Shizuoka | Aichi | Mie | Shiga
  | Kyoto | Osaka | Hyogo | Nara | Wakayama
  | Tottori | Shimane | Okayama | Hiroshima | Yamaguchi
  | Tokushima | Kagawa | Ehime | Kochi
  | Fukuoka | Saga | Nagasaki | Kumamoto | Oita | Miyazaki | Kagoshima {- (excluding south west islands) -}
  | Okinawa
  | Islands_Tokyo -- part of Tokyo (Izu,Ogasawara islands)
  | Islands_Kagoshima --  part of Kagoshima
  deriving (Eq,Enum,Bounded,Show)

bitmaplen = Prelude.length [(minBound :: TargetArea) .. (maxBound :: TargetArea)]

instance FromWord64.Class [TargetArea] where
  fromWord64 x = Prelude.filter (\ta -> (testBit x) $ ((bitmaplen-1) -) $ fromEnum ta ) [(minBound :: TargetArea) .. (maxBound :: TargetArea)]

class Class a where
  pefecture_bitmap :: a -> [TargetArea] -- 56

data Data = MkData {
  _pefecture_bitmap :: Word64 -- 56
  } deriving (Show)

instance Class Data where
  pefecture_bitmap = fromWord64 . _pefecture_bitmap
