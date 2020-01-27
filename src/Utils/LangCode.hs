{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.LangCode where
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BChar8
import qualified Utils.FromByteString as FromByteString
import qualified Utils.ToString as ToString


data Data = MkData Char Char Char deriving (Eq,Show)

instance FromByteString.Class Data where
  fromByteStringWithRest x = (\(y,z) -> (impl' $ BChar8.unpack y,z)) $ BS.splitAt 3 x
    where
      impl' []                = MkData '\0' '\0' '\0'
      impl' (x1:[])           = MkData '\0' '\0' x1
      impl' (x1:(x2:[]))      = MkData '\0' x1   x2
      impl' (x1:(x2:(x3:xs))) = MkData x1   x2   x3

instance ToString.Class Data where
  toString (MkData a b c) =  [a,b,c]
