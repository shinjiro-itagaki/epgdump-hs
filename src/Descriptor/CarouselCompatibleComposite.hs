module Descriptor.CarouselCompatibleComposite (
  module Descriptor.CarouselCompatibleComposite.Expire
  ,module Descriptor.CarouselCompatibleComposite.Name
  ,module Descriptor.CarouselCompatibleComposite.ProviderPrivate
  ,module Descriptor.CarouselCompatibleComposite.StoreRoot
  ,module Descriptor.CarouselCompatibleComposite.Subdirectory
  ,module Descriptor.CarouselCompatibleComposite.Title
  ,module Descriptor.CarouselCompatibleComposite.Type
  ) where
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

import Descriptor.CarouselCompatibleComposite.Expire
import Descriptor.CarouselCompatibleComposite.Name
import Descriptor.CarouselCompatibleComposite.ProviderPrivate
import Descriptor.CarouselCompatibleComposite.StoreRoot
import Descriptor.CarouselCompatibleComposite.Subdirectory
import Descriptor.CarouselCompatibleComposite.Title
import Descriptor.CarouselCompatibleComposite.Type
 
class Base a => CarouselCompatibleComposite a where
  sub_descriptors :: a -> [SubDescriptorData]

data SubDescriptorData =
  MkStoreRoot StoreRootData
  | MkSubdirectory SubdirectoryData
  | MkTypeData TypeData
  | MkNameData NameData
  | MkExpireData ExpireData
  | MkProviderPrivateData ProviderPrivateData
  | MkTitleData TitleData
