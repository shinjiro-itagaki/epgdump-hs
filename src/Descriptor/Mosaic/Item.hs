{-# LANGUAGE FlexibleInstances #-}

module Descriptor.Mosaic.Item where
import qualified Descriptor.Mosaic.ElementaryCellField as ElementaryCellField
import qualified Descriptor.Mosaic.CellLinkageInfo as CellLinkageInfo
import Data.Vector(Vector,toList,empty,snoc)
import qualified Data.Vector as V
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.FromByteString as FromByteString
import qualified Data.ByteString.Lazy as BS
import Utils

class (FromByteString.Class a, CellLinkageInfo.Class a) => Class a where
  logical_cell_id                :: a -> Word8 -- 6
  reserved_future_use            :: a -> Word8 -- 7
  logical_cell_presentation_info :: a -> Word8 -- 3
  elementary_cell_field_length   :: a -> Word8 -- 8
  elementary_cell_fields         :: a -> Vector ElementaryCellField.Data

data Data = MkData {
  _logical_cell_id                :: Word8, -- 6
  _reserved_future_use            :: Word8, -- 7
  _logical_cell_presentation_info :: Word8, -- 3
  _elementary_cell_field_length   :: Word8, -- 8
  _elementary_cell_fields         :: Vector ElementaryCellField.Data,
  _cell_linkage                   :: CellLinkageInfo.Data -- 8 + n
  } deriving (Show,Eq)

instance CellLinkageInfo.Class Data where
  cell_linkage = _cell_linkage

instance Class Data where
  logical_cell_id                = _logical_cell_id
  reserved_future_use            = _reserved_future_use
  logical_cell_presentation_info = _logical_cell_presentation_info
  elementary_cell_field_length   = _elementary_cell_field_length
  elementary_cell_fields         = _elementary_cell_fields

instance FromByteString.Class Data where
  fromByteStringWithRest bs0 =
    let (w16,bs1)                           = fromByteStringWithRest bs0 :: (Word16,ByteString)
        logical_cell_id                     = toWord8 $ (`shiftR` 10) $ w16 .&. 0xF600 -- 6
        reserved_future_use                 = toWord8 $ (`shiftR`  3) $ w16 .&. 0x03F8 -- 7 
        logical_cell_presentation_info      = toWord8 $ (`shiftR`  0) $ w16 .&. 0x0007 -- 3
        (elementary_cell_field_length ,bs2) = fromByteStringWithRest bs1
        (bs3,bs4)                           = BS.splitAt (fromInteger $ toInteger elementary_cell_field_length) bs2
        elementary_cell_fields              = V.map ElementaryCellField.mk $ BS.foldl snoc V.empty bs3 
        (cell_linkage,bs5)                  = fromByteStringWithRest bs4
        d = MkData {
          _reserved_future_use            = reserved_future_use,
          _logical_cell_presentation_info = logical_cell_presentation_info,
          _elementary_cell_field_length   = elementary_cell_field_length,
          _elementary_cell_fields         = elementary_cell_fields,
          _cell_linkage                   = cell_linkage
          }
    in (d,bs5)

instance EmptyExist.Class (V.Vector Data) where
  mkEmpty = V.empty
