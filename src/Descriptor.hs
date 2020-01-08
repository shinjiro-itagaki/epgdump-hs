{-# LANGUAGE FlexibleInstances #-}

module Descriptor (
  module Descriptor.Common
  -- module Descriptor.AVC_Timing_HRD,
  -- module Descriptor.AVC_Video,
  -- module Descriptor.AudioComponent,
  -- module Descriptor.BoardInformation,
  -- module Descriptor.BouquetName,
  -- module Descriptor.BroadcasterName,
  -- module Descriptor.CAIdentifier,
  -- module Descriptor.CarouselCompatibleComposite,
  -- module Descriptor.Component,
  -- module Descriptor.ComponentControl,
  -- module Descriptor.ComponentGroup,
  -- module Descriptor.ConditionalAccess,
  -- module Descriptor.ConditionalPlayback,
  -- module Descriptor.ConnectedTransmission,
  -- module Descriptor.Content,
  -- module Descriptor.ContentAvailability,
  -- module Descriptor.CountryAvailability,
  -- module Descriptor.DataComponent,
  -- module Descriptor.DataContents,
  -- module Descriptor.DigitalCopyControl,
  -- module Descriptor.EmergencyInformation,
  -- module Descriptor.EventGroup,
  -- module Descriptor.ExtendedBroadcaster,
  -- module Descriptor.ExtendedEvent,
  -- module Descriptor.HierarchicalTransmission,
  -- module Descriptor.HyperLink,
  -- module Descriptor.LDT_Link,
  -- module Descriptor.Linkage,
  -- module Descriptor.LocalTimeOffset,
  -- module Descriptor.LogoTransmission,
  -- module Descriptor.Mosaic,
  -- module Descriptor.NVOD_Reference,
  -- module Descriptor.NetworkName,
  -- module Descriptor.ParentalRating,
  -- module Descriptor.PartialReception,
  -- module Descriptor.SI_Parameter,
  -- module Descriptor.SI_Prime_TS,
  -- module Descriptor.SatelliteDeliverySystem,
  -- module Descriptor.Series,
  -- module Descriptor.Service,
  -- module Descriptor.ServiceGroup,
  -- module Descriptor.ServiceList,
  -- module Descriptor.ShortEvent,
  -- module Descriptor.StreamIdentifier,
  -- module Descriptor.Stuffing,
  -- module Descriptor.SystemManagement,
  -- module Descriptor.TS_Information,
  -- module Descriptor.TargetRegion,
  -- module Descriptor.TerrestrialDeliverySystem,
  -- module Descriptor.TimeShiftedEvent,
  -- module Descriptor.TimeShiftedService,
  -- module Descriptor.VideoDecodeControl
  ) where

import Descriptor.Common
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
import Descriptor.LDT_Link
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

import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
-- import Descriptor.Class(HasOriginalNetworkID,TOS,HasServiceID)

data Desc =
  ParseFailed
  | MkAVC_Timing_HRD             Descriptor.AVC_Timing_HRD.Data
  | MkAVC_Video                  Descriptor.AVC_Video.Data
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
  | LDT_Link                     Descriptor.LDT_Link.Data
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
  
parse :: ByteString -> (Desc,ByteString)
parse bs = (ParseFailed, bs) 
