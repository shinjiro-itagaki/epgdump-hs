{-# LANGUAGE FlexibleInstances #-}

module Descriptor where

import Descriptor.AVC_Timing_HRD
import Descriptor.AVC_Video
import Descriptor.AudioComponent
import Descriptor.BoardInformation
import Descriptor.BouquetName
import Descriptor.BroadcasterName
import Descriptor.CAIdentifier
import Descriptor.CarouselCompatibleComposite
import Descriptor.Component
import Descriptor.ComponentControl
import Descriptor.ComponentGroup
import Descriptor.ConditionalAccess
import Descriptor.ConditionalPlayback
import Descriptor.ConnectedTransmission
import Descriptor.Content
import Descriptor.ContentAvailability
import Descriptor.CountryAvailability
import Descriptor.DataComponent
import Descriptor.DataContents
import Descriptor.DigitalCopyControl
import Descriptor.EmergencyInformation
import Descriptor.EventGroup
import Descriptor.ExtendedBroadcaster
import Descriptor.ExtendedEvent
import Descriptor.HierarchicalTransmission
import Descriptor.HyperLink
import Descriptor.LDT_Linkage
import Descriptor.Linkage
import Descriptor.LocalTimeOffset
import Descriptor.LogoTransmission
import Descriptor.Mosaic
import Descriptor.NVOD_Reference
import Descriptor.NetworkName
import Descriptor.ParentalRating
import Descriptor.PartialReception
import Descriptor.SI_Parameter
import Descriptor.SI_Prime_TS
import Descriptor.SatelliteDeliverySystem
import Descriptor.Series
import Descriptor.Service
import Descriptor.ServiceGroup
import Descriptor.ServiceList
import Descriptor.ShortEvent
import Descriptor.StreamIdentifier
import Descriptor.Stuffing
import Descriptor.SystemManagement
import Descriptor.TS_Information
import Descriptor.TargetRegion
import Descriptor.TerrestrialDeliverySystem
import Descriptor.TimeShiftedEvent
import Descriptor.TimeShiftedService
import Descriptor.VideoDecodeControl

import qualified Descriptor.Header as Header
import qualified Data.ByteString.Lazy as BS
import qualified Parser.Result as Result
import qualified BytesReader.Base as BytesReaderBase
import qualified BytesReader.Counter as Counter
import qualified Utils.FromByteString as FromByteString
import Utils
import qualified Utils.EmptyExist as EmptyExist
import qualified Descriptor.Base as Base

data Data =
  Null
  | NotSupported
  | ParseFailed
  | AVC_Timing_HRD               Descriptor.AVC_Timing_HRD.Data
  | AVC_Video                    Descriptor.AVC_Video.Data
  | AudioComponent               Descriptor.AudioComponent.Data
  | BoardInformation             Descriptor.BoardInformation.Data
  | BouquetName                  Descriptor.BouquetName.Data
  | BroadcasterName              Descriptor.BroadcasterName.Data
  | CAIdentifier                 Descriptor.CAIdentifier.Data
  | CarouselCompatibleComposite  Descriptor.CarouselCompatibleComposite.Data
  | Component                    Descriptor.Component.Data
  | ComponentControl             Descriptor.ComponentControl.Data
  | ComponentGroup               Descriptor.ComponentGroup.Data
  | ConditionalAccess            Descriptor.ConditionalAccess.Data
  | ConditionalPlayback          Descriptor.ConditionalPlayback.Data
  | ConnectedTransmission        Descriptor.ConnectedTransmission.Data
  | Content                      Descriptor.Content.Data
  | ContentAvailability          Descriptor.ContentAvailability.Data
  | CountryAvailability          Descriptor.CountryAvailability.Data
  | DataComponent                Descriptor.DataComponent.Data
  | DataContents                 Descriptor.DataContents.Data
  | DigitalCopyControl           Descriptor.DigitalCopyControl.Data
  | EmergencyInformation         Descriptor.EmergencyInformation.Data
  | EventGroup                   Descriptor.EventGroup.Data
  | ExtendedBroadcaster          Descriptor.ExtendedBroadcaster.Data
  | ExtendedEvent                Descriptor.ExtendedEvent.Data
  | HierarchicalTransmission     Descriptor.HierarchicalTransmission.Data
  | HyperLink                    Descriptor.HyperLink.Data
  | LDT_Linkage                  Descriptor.LDT_Linkage.Data
  | Linkage                      Descriptor.Linkage.Data
  | LocalTimeOffset              Descriptor.LocalTimeOffset.Data
  | LogoTransmission             Descriptor.LogoTransmission.Data
  | Mosaic                       Descriptor.Mosaic.Data
  | NVOD_Reference               Descriptor.NVOD_Reference.Data
  | NetworkName                  Descriptor.NetworkName.Data
  | ParentalRating               Descriptor.ParentalRating.Data
  | PartialReception             Descriptor.PartialReception.Data
  | SI_Parameter                 Descriptor.SI_Parameter.Data
  | SI_Prime_TS                  Descriptor.SI_Prime_TS.Data
  | SatelliteDeliverySystem      Descriptor.SatelliteDeliverySystem.Data
  | Series                       Descriptor.Series.Data
  | Service                      Descriptor.Service.Data
  | ServiceGroup                 Descriptor.ServiceGroup.Data
  | ServiceList                  Descriptor.ServiceList.Data
  | ShortEvent                   Descriptor.ShortEvent.Data
  | StreamIdentifier             Descriptor.StreamIdentifier.Data
  | Stuffing                     Descriptor.Stuffing.Data
  | SystemManagement             Descriptor.SystemManagement.Data
  | TS_Information               Descriptor.TS_Information.Data
  | TargetRegion                 Descriptor.TargetRegion.Data
  | TerrestrialDeliverySystem    Descriptor.TerrestrialDeliverySystem.Data
  | TimeShiftedEvent             Descriptor.TimeShiftedEvent.Data
  | TimeShiftedService           Descriptor.TimeShiftedService.Data
  | VideoDecodeControl           Descriptor.VideoDecodeControl.Data
  deriving (Show)  

instance EmptyExist.Class Descriptor.Data where
  mkEmpty = Null

map_ :: (Show a, Show b) => (a -> b) -> Result.Data a -> Result.Data b   -- ここを省略すると型が単一に固定されるためコンパイルが通らない
map_ = Result.map

instance FromByteString.Class Descriptor.Data where
  fromByteStringWithRest bs =
    let (header,bs0) = fromByteStringWithRest bs
        (bs1,rest) = BS.splitAt (Header.descriptor_length header) bs0
        res = (\f -> f bs1) $ case Header.descriptor_tag header of
                0x2A -> map_ AVC_Timing_HRD              . Base.fromByteStringAfterHeader header
                0x28 -> map_ AVC_Video                   . Base.fromByteStringAfterHeader header
                0xC4 -> map_ AudioComponent              . Base.fromByteStringAfterHeader header
                0xDB -> map_ BoardInformation            . Base.fromByteStringAfterHeader header
                0x47 -> map_ BouquetName                 . Base.fromByteStringAfterHeader header
                0xD8 -> map_ BroadcasterName             . Base.fromByteStringAfterHeader header
                0x53 -> map_ CAIdentifier                . Base.fromByteStringAfterHeader header
                0xF7 -> map_ CarouselCompatibleComposite . Base.fromByteStringAfterHeader header
                0x50 -> map_ Component                   . Base.fromByteStringAfterHeader header
                0x00 -> map_ ComponentControl            . Base.fromByteStringAfterHeader header
                0xD9 -> map_ ComponentGroup              . Base.fromByteStringAfterHeader header
                0x09 -> map_ ConditionalAccess           . Base.fromByteStringAfterHeader header
                0xF8 -> map_ ConditionalPlayback         . Base.fromByteStringAfterHeader header
                0xDD -> map_ ConnectedTransmission       . Base.fromByteStringAfterHeader header
                0x54 -> map_ Content                     . Base.fromByteStringAfterHeader header
                0xDE -> map_ ContentAvailability         . Base.fromByteStringAfterHeader header
                0x49 -> map_ CountryAvailability         . Base.fromByteStringAfterHeader header
                0xFD -> map_ DataComponent               . Base.fromByteStringAfterHeader header
                0xC7 -> map_ DataContents                . Base.fromByteStringAfterHeader header
                0xC1 -> map_ DigitalCopyControl          . Base.fromByteStringAfterHeader header
                0xFC -> map_ EmergencyInformation        . Base.fromByteStringAfterHeader header
                0xD6 -> map_ EventGroup                  . Base.fromByteStringAfterHeader header
                0xCE -> map_ ExtendedBroadcaster         . Base.fromByteStringAfterHeader header
                0x4E -> map_ ExtendedEvent               . Base.fromByteStringAfterHeader header
                0xC0 -> map_ HierarchicalTransmission    . Base.fromByteStringAfterHeader header
                0xC5 -> map_ HyperLink                   . Base.fromByteStringAfterHeader header
                0xDC -> map_ LDT_Linkage                 . Base.fromByteStringAfterHeader header
                0x4A -> map_ Linkage                     . Base.fromByteStringAfterHeader header
                0x58 -> map_ LocalTimeOffset             . Base.fromByteStringAfterHeader header
                0xCF -> map_ LogoTransmission            . Base.fromByteStringAfterHeader header
                0x51 -> map_ Mosaic                      . Base.fromByteStringAfterHeader header
                0x4B -> map_ NVOD_Reference              . Base.fromByteStringAfterHeader header
                0x40 -> map_ NetworkName                 . Base.fromByteStringAfterHeader header
                0x55 -> map_ ParentalRating              . Base.fromByteStringAfterHeader header
                0xFB -> map_ PartialReception            . Base.fromByteStringAfterHeader header
                0xD7 -> map_ SI_Parameter                . Base.fromByteStringAfterHeader header
                0xDA -> map_ SI_Prime_TS                 . Base.fromByteStringAfterHeader header
                0x43 -> map_ SatelliteDeliverySystem     . Base.fromByteStringAfterHeader header
                0xD5 -> map_ Series                      . Base.fromByteStringAfterHeader header
                0x48 -> map_ Service                     . Base.fromByteStringAfterHeader header
                0xE0 -> map_ ServiceGroup                . Base.fromByteStringAfterHeader header
                0x41 -> map_ ServiceList                 . Base.fromByteStringAfterHeader header
                0x4D -> map_ ShortEvent                  . Base.fromByteStringAfterHeader header
                0x52 -> map_ StreamIdentifier            . Base.fromByteStringAfterHeader header
                0x42 -> map_ Stuffing                    . Base.fromByteStringAfterHeader header
                0xFE -> map_ SystemManagement            . Base.fromByteStringAfterHeader header
                0xCD -> map_ TS_Information              . Base.fromByteStringAfterHeader header
                0xC6 -> map_ TargetRegion                . Base.fromByteStringAfterHeader header
                0xFA -> map_ TerrestrialDeliverySystem   . Base.fromByteStringAfterHeader header
                0x4F -> map_ TimeShiftedEvent            . Base.fromByteStringAfterHeader header
                0x4C -> map_ TimeShiftedService          . Base.fromByteStringAfterHeader header
                0xC8 -> map_ VideoDecodeControl          . Base.fromByteStringAfterHeader header
                _    -> (\x -> Result.NotSupported)
    in case res of
      Result.Parsed     x -> (x           ,rest)
      Result.NotSupported -> (NotSupported,rest)
      _                   -> (ParseFailed ,rest)

