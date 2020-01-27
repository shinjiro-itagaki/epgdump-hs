{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.EIT(
  Data,
  Class(..),
  ) where

import qualified SITables.Base as Base
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import Utils
import qualified Descriptor
import Data.Vector(Vector,toList,empty,snoc)
import Data.Maybe(fromMaybe)
import qualified SITables.EIT.Item as Item
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.SITableIDs as SITableIDs

class (Base.Class a, ServiceInfo.Class a) => Class a where
  segment_last_section_number :: a -> Word8
  last_table_id               :: a -> Word8
  items                       :: a -> [Item.Data]

data Data = MkData {
  _header1                     :: Header1.Data,
  _service_info                :: ServiceInfo.Data,
  _header2                     :: Header2.Data,
  _segment_last_section_number :: Word8,
  _last_table_id               :: Word8,
  _items                       :: Vector Item.Data, 
  _footer                      :: Footer.Data
  } deriving (Show)

instance ServiceInfo.Class Data where
  service_info = _service_info
  
instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty empty mkEmpty

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance Footer.Class Data where
  footer = _footer

instance SITableIDs.Class Data where
  pids      _ = MkPIDs [0x0012,0x0026,0x0027]
  table_ids _ = [0x4E,0x4F] ++ [0x50..0x5F] ++ [0x60..0x6F]

instance Class Data where
  segment_last_section_number = _segment_last_section_number
  last_table_id               = _last_table_id

instance Base.Class Data where
  footer  = Just . _footer
  parseAfterHeader1 h bs =
    let (footer,bs0) = fromByteStringWithRest bs
        ((service_id,
          header2,
          transport_stream_id,
          original_network_id,
          segment_last_section_number,
          last_table_id,
          items
         ),rest) = fromByteStringWithRest bs0
        d = MkData {
          _header1                     = h,
          _service_info                = ServiceInfo.mk original_network_id transport_stream_id service_id,
          _header2                     = header2,
          _segment_last_section_number = segment_last_section_number,
          _last_table_id               = last_table_id,
          _items                       = items,
          _footer                      = footer
          }
    in Result.Parsed d
