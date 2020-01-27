{-# LANGUAGE FlexibleInstances #-}
module Utils.FromByteString where
import Common(ByteString)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import qualified Data.ByteString.Lazy as BS
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy.Char8 as BChar8

import Data.Int(Int64)
import Utils.ToWord64(toWord64)
import Utils.ToWord32(toWord32)
import Utils.ToWord16(toWord16)
import Utils.ToWord8(toWord8)
import Data.Maybe(fromMaybe)

import qualified Utils.Collection as Collection
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Utils.EmptyExist as EmptyExist

map :: (a -> b) -> (a,ByteString) -> (b,ByteString)
map f (x,y) = (f x,y)

class Class a where
  fromByteStringWithRest :: ByteString -> (a,ByteString)
  
  fromByteStringWithRest_n :: (Ord n,Num n) => n -> ByteString -> ([a],ByteString)
  fromByteStringWithRest_n n bs = impl n bs []
    where
      impl n' bs' xs'
        | BS.length bs' < 1 || n' < 1 = (xs',bs')
        | otherwise =
            let (x',rest') = fromByteStringWithRest bs'
            in impl (n'-1) rest' (xs' ++ [x'])
        
  fromByteString :: ByteString -> a
  fromByteString = fst . fromByteStringWithRest

  fromByteString_n :: (Ord n,Num n) => n -> ByteString -> [a]
  fromByteString_n n bs = fst $ fromByteStringWithRest_n n bs
  
  gatherFromByteString :: (EmptyExist.Class b) => ByteString -> (b -> a -> b) -> b
  gatherFromByteString bs appender = impl bs appender EmptyExist.mkEmpty
    where
      impl bs appender y
        | BS.null bs = y
        | otherwise = let (e,rest) = fromByteStringWithRest bs
                      in impl rest appender (appender y e)

  fromByteStringWithRestM :: (Monad m) => ByteString -> (m a,ByteString)
  fromByteStringWithRestM xs =
    let (v,rest) = fromByteStringWithRest xs
    in (return v,rest)

instance (Class a0,Class a1) => Class (a0,a1) where
  fromByteStringWithRest bs =
    let (v0,bs0) = fromByteStringWithRest bs
        (v1,bs1) = fromByteStringWithRest bs0
    in ((v0,v1),bs1)

instance (Class a0,Class a1,Class a2) => Class (a0,a1,a2) where
  fromByteStringWithRest bs =
    let (v0,bs0) = fromByteStringWithRest bs
        (v1,bs1) = fromByteStringWithRest bs0
        (v2,bs2) = fromByteStringWithRest bs1
    in ((v0,v1,v2),bs2)
    
instance (Class a0,Class a1,Class a2,Class a3) => Class (a0,a1,a2,a3) where
  fromByteStringWithRest bs =
    let (v0,bs0) = fromByteStringWithRest bs
        (v1,bs1) = fromByteStringWithRest bs0
        (v2,bs2) = fromByteStringWithRest bs1
        (v3,bs3) = fromByteStringWithRest bs2
    in ((v0,v1,v2,v3),bs3)

instance (Class a0,Class a1,Class a2,Class a3,Class a4) => Class (a0,a1,a2,a3,a4) where
  fromByteStringWithRest bs =
    let (v0,bs0) = fromByteStringWithRest bs
        (v1,bs1) = fromByteStringWithRest bs0
        (v2,bs2) = fromByteStringWithRest bs1
        (v3,bs3) = fromByteStringWithRest bs2
        (v4,bs4) = fromByteStringWithRest bs3      
    in ((v0,v1,v2,v3,v4),bs4)

instance (Class a0,Class a1,Class a2,Class a3,Class a4,Class a5) => Class (a0,a1,a2,a3,a4,a5) where
  fromByteStringWithRest bs =
    let (v0,bs0) = fromByteStringWithRest bs
        (v1,bs1) = fromByteStringWithRest bs0
        (v2,bs2) = fromByteStringWithRest bs1
        (v3,bs3) = fromByteStringWithRest bs2
        (v4,bs4) = fromByteStringWithRest bs3
        (v5,bs5) = fromByteStringWithRest bs4
    in ((v0,v1,v2,v3,v4,v5),bs5)

instance (Class a0,Class a1,Class a2,Class a3,Class a4,Class a5,Class a6) => Class (a0,a1,a2,a3,a4,a5,a6) where
  fromByteStringWithRest bs =
    let (v0,bs0) = fromByteStringWithRest bs
        (v1,bs1) = fromByteStringWithRest bs0
        (v2,bs2) = fromByteStringWithRest bs1
        (v3,bs3) = fromByteStringWithRest bs2
        (v4,bs4) = fromByteStringWithRest bs3
        (v5,bs5) = fromByteStringWithRest bs4
        (v6,bs6) = fromByteStringWithRest bs5
    in ((v0,v1,v2,v3,v4,v5,v6),bs6)

instance (Class a0,Class a1,Class a2,Class a3,Class a4,Class a5,Class a6,Class a7) => Class (a0,a1,a2,a3,a4,a5,a6,a7) where
  fromByteStringWithRest bs =
    let (v0,bs0) = fromByteStringWithRest bs
        (v1,bs1) = fromByteStringWithRest bs0
        (v2,bs2) = fromByteStringWithRest bs1
        (v3,bs3) = fromByteStringWithRest bs2
        (v4,bs4) = fromByteStringWithRest bs3
        (v5,bs5) = fromByteStringWithRest bs4
        (v6,bs6) = fromByteStringWithRest bs5
        (v7,bs7) = fromByteStringWithRest bs6
    in ((v0,v1,v2,v3,v4,v5,v6,v7),bs7)

instance Class Word64 where
  fromByteStringWithRest x = (\(y,z) -> (toWord64 $ BS.unpack y,z)) $ BS.splitAt 8 x

instance Class Word32 where
  fromByteStringWithRest x = (\(y,z) -> (toWord32 $ BS.unpack y,z)) $ BS.splitAt 4 x

instance Class Word16 where
  fromByteStringWithRest x = (\(y,z) -> (toWord16 $ BS.unpack y,z)) $ BS.splitAt 2 x

instance Class Word8 where
  fromByteStringWithRest x = (\(y,z) -> (toWord8  $ BS.unpack y,z)) $ BS.splitAt 1 x

instance Class Char where
  fromByteStringWithRest = fromMaybe ('\0',BS.empty) . BChar8.uncons

instance (Class a) => Class (V.Vector a) where
  fromByteStringWithRest bs = impl bs V.empty
    where
      impl bs' v
        | BS.null bs' = (v,bs')
        | otherwise =
            let (e,rest) = fromByteStringWithRest bs'
            in impl rest (V.snoc v e)
        


