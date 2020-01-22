{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.AdaptationField.OptionalFields where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import Common(ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Parser.Result as Result
import Parser.Result((>>==))

type Flags3 = (Bool,Bool,Bool)

class Class a where
  pcr                               :: a -> Word64 -- 42
  opcr                              :: a -> Word64 -- 42
  splice_countdown                  :: a -> Word8  -- 8
  transport_private_data_length     :: a -> Word8 -- 8
  transport_private_data            :: a -> ByteString -- depends on transport_private_data_length
  adaptation_field_extension_length :: a -> Word8 -- 8
  flags3                            :: a -> Flags3 -- 3
  ltw_valid_flag                    :: a -> Bool -- 1
  ltw_offset                        :: a -> Word16 -- 15
  no_name                           :: a -> (Bool,Bool) -- 2
  piecewise_rate                    :: a -> Word32 -- 22 , 5
  splice_type                       :: a -> Word8 -- 4
  dts_next_au                       :: a -> Word64 -- 33 ,   
  fromByteString                    :: [Word8] -> (Result.Data a,[Word8])

data Data = MkData {
  _pcr                               :: Word64, -- 42 5.25
  _opcr                              :: Word64, -- 42 10.5
  _splice_countdown                  :: Word8,  --  8 15
  _transport_private_data_length     :: Word8,  --  8 16
  _transport_private_data            :: ByteString, -- 8n n depends on transport_private_data_length
  _adaptation_field_extension_length :: Word8, -- 8
  _flags3                            :: Flags3, -- 3 , 3
  _ltw_valid_flag                    :: Bool, -- 1 , 4
  _ltw_offset                        :: Word16, -- 15 , 19
  _no_name                           :: (Bool,Bool), -- 2 , 21
  _piecewise_rate                    :: Word32, -- 22 , 43
  _splice_type                       :: Word8, -- 4 , 47
  _dts_next_au                       :: Word64 -- 33, 80
  } deriving(Eq,Show)

mkEmpty = MkData {
  _pcr                               = 0,
  _opcr                              = 0,
  _splice_countdown                  = 0,
  _transport_private_data_length     = 0,
  _transport_private_data            = BS.empty,
  _adaptation_field_extension_length = 0,
  _flags3                            = (False,False,False),
  _ltw_valid_flag                    = False,
  _ltw_offset                        = 0,
  _no_name                           = (False,False),
  _piecewise_rate                    = 0,
  _splice_type                       = 0,
  _dts_next_au                       = 0
  }

instance Class Data where
  pcr                               = _pcr
  opcr                              = _opcr
  splice_countdown                  = _splice_countdown
  transport_private_data_length     = _transport_private_data_length
  transport_private_data            = _transport_private_data
  adaptation_field_extension_length = _adaptation_field_extension_length
  flags3                            = _flags3
  ltw_valid_flag                    = _ltw_valid_flag
  ltw_offset                        = _ltw_offset
  no_name                           = _no_name
  piecewise_rate                    = _piecewise_rate
  splice_type                       = _splice_type
  dts_next_au                       = _dts_next_au  
  fromByteString xs =
    (Result.Parsed mkEmpty,[])

