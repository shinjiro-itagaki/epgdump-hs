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

import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO)
import qualified Parser
import qualified Parser.Result as Result
import Common(EmptyExist(..),BytesLen,BitsLen)
import qualified BytesReader.HolderIO as HolderIO
import qualified BytesReader.Counter as Counter


data Data =
  Null
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

instance EmptyExist Descriptor.Data where
  mkEmpty = Null

instance Parser.Class Descriptor.Data where
--  parseIOFlow = flowStart

gather :: (HolderIO.Class bh, Parser.Class b, Show b) => (b -> Descriptor.Data -> b) -> BytesLen -> bh -> b -> IO (ParseResult b, bh)
gather appender restlen fh init
  | restlen < 1 = return (Result.Parsed init, fh)
  | otherwise = do
      res@(res_item,fh') <- parseIO fh
      case res_item of
        Result.Parsed item -> gather appender (restlen - ((Counter.getBytesCounter fh') - (Counter.getBytesCounter fh))) fh' (appender init item)
        _           -> return $ (\x -> (x,fh')) $ mapParseResult (\_ -> init) res_item

