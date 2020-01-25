module Utils.ComponentType where

import Data.Word(Word64, Word32, Word16, Word8)

data Data = ReservedForFutureUse | Video VideoSize VideoOutPut AspectRatio PanVector | Audio AudioMode | UserDefined deriving (Show,Eq)

data AspectRatio = Ratio_4_3 | Ratio_16_9 | Ratio_Over_16_9 deriving (Show,Eq)
data VideoOutPut = P | I deriving (Show,Eq) -- progressive / interlace
data VideoSize   = S240 | S480 | S720 | S1080 deriving (Show,Eq)
data PanVector = WithPanVector | WithoutPanVector | Undef deriving (Show,Eq)
data AudioMode = SingleMono {- Mode_1_0 -} | DualMono {- Mode_1_0_1_0 -} | Stereo {- Mode_2_0 -} | Mode_2_1 | Mode_3_0 | Mode_2_2 | Mode_3_1 | Mode_3_2 | Mode_3_2_LFE | VisuallyImpired | HardOfHearing deriving (Show,Eq)

type StreamContent = Word8
type RawData = Word8

toData :: StreamContent -> RawData -> Data
toData 0x01 0x01 = Video S480 I Ratio_4_3       Undef
toData 0x01 0x02 = Video S480 I Ratio_16_9      WithPanVector
toData 0x01 0x03 = Video S480 I Ratio_16_9      WithoutPanVector
toData 0x01 0x04 = Video S480 I Ratio_Over_16_9 Undef

toData 0x01 0xA1 = Video S480 P Ratio_4_3       Undef
toData 0x01 0xA2 = Video S480 P Ratio_16_9      WithPanVector
toData 0x01 0xA3 = Video S480 P Ratio_16_9      WithoutPanVector
toData 0x01 0xA4 = Video S480 P Ratio_Over_16_9 Undef

toData 0x01 0xB1 = Video S1080 I Ratio_4_3       Undef
toData 0x01 0xB2 = Video S1080 I Ratio_16_9      WithPanVector
toData 0x01 0xB3 = Video S1080 I Ratio_16_9      WithoutPanVector
toData 0x01 0xB4 = Video S1080 I Ratio_Over_16_9 Undef

toData 0x01 0xC1 = Video S720 P Ratio_4_3       Undef
toData 0x01 0xC2 = Video S720 P Ratio_16_9      WithPanVector
toData 0x01 0xC3 = Video S720 P Ratio_16_9      WithoutPanVector
toData 0x01 0xC4 = Video S720 P Ratio_Over_16_9 Undef

toData 0x01 0xD1 = Video S240 P Ratio_4_3       Undef
toData 0x01 0xD2 = Video S240 P Ratio_16_9      WithPanVector
toData 0x01 0xD3 = Video S240 P Ratio_16_9      WithoutPanVector
toData 0x01 0xD4 = Video S240 P Ratio_Over_16_9 Undef

toData 0x02 0x01 = Audio SingleMono
toData 0x02 0x02 = Audio DualMono
toData 0x02 0x03 = Audio Stereo
toData 0x02 0x04 = Audio Mode_2_1
toData 0x02 0x05 = Audio Mode_3_0
toData 0x02 0x06 = Audio Mode_2_2
toData 0x02 0x07 = Audio Mode_3_1
toData 0x02 0x08 = Audio Mode_3_2
toData 0x02 0x09 = Audio Mode_3_2_LFE

toData 0x02 0x40 = Audio VisuallyImpired
toData 0x02 0x41 = Audio HardOfHearing

toData 0x02 x
  | 0xB0 <= x && x <= 0xFE = UserDefined
  | otherwise              = ReservedForFutureUse

toData x _
  | 0x03 <= x && x <= 0x0B = ReservedForFutureUse
  | 0x0C <= x && x <= 0x0F = UserDefined
  | otherwise              = ReservedForFutureUse
