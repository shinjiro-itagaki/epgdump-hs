module Descriptor.Link.ServiceInfo (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString,EmptyExist(..))
import qualified Utils.FromByteString as FromByteString
import Utils.FromByteString(fromByteStringWithRest)

class (EmptyExist a) => Class a where
  service_info :: a -> Data
  
  id0 :: a -> Word16
  id0 = id0 . service_info
  
  id1 :: a -> Word16
  id1 = id1 . service_info
  
  id2 :: a -> Word16
  id2 = id2 . service_info  
  
  original_network_id :: a -> Word16
  original_network_id = id0
  
  transport_stream_id :: a -> Word16
  transport_stream_id = id1
  
  service_id :: a -> Word16
  service_id = id2
  
data Data = MkData {
  _id0 {- _original_network_id -} :: Word16,  
  _id1 {- _transport_stream_id -} :: Word16,  
  _id2 {- _service_id -}          :: Word16
  } deriving (Show,Eq) -- 0x01

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty

instance FromByteString.Class Data where
  fromByteStringWithRest bs0 =
    let (id0 ,bs1) = fromByteStringWithRest bs0
        (id1 ,bs2) = fromByteStringWithRest bs1
        (id2 ,bs3) = fromByteStringWithRest bs2
        d = MkData {
          _id0 = id0,
          _id1 = id1,
          _id2 = id2
          }
    in (d, bs3)

instance Class Data where
  service_info x = x
  id0 = _id0
  id1 = _id1
  id2 = _id2

