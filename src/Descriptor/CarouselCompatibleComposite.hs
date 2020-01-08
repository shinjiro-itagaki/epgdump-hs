module Descriptor.CarouselCompatibleComposite (
  Data,Class(..)
  ,module SubExpire
  ,module SubName
  ,module SubProviderPrivate 
  ,module SubStoreRoot 
  ,module SubSubdirectory
  ,module SubTitle
  ,module SubType  
  ) where
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

import qualified Descriptor.CarouselCompatibleComposite.Expire          as SubExpire
import qualified Descriptor.CarouselCompatibleComposite.Name            as SubName
import qualified Descriptor.CarouselCompatibleComposite.ProviderPrivate as SubProviderPrivate 
import qualified Descriptor.CarouselCompatibleComposite.StoreRoot       as SubStoreRoot 
import qualified Descriptor.CarouselCompatibleComposite.Subdirectory    as SubSubdirectory
import qualified Descriptor.CarouselCompatibleComposite.Title           as SubTitle
import qualified Descriptor.CarouselCompatibleComposite.Type            as SubType
 
class Base a => Class a where
  sub_descriptors :: a -> [SubDescriptorData]

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _sub_descriptors   :: [SubDescriptorData]  
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where  
  sub_descriptors = _sub_descriptors
  
data SubDescriptorData =
  MkStoreRoot         SubStoreRoot.Data
  | MkSubdirectory    SubSubdirectory.Data
  | MkType            SubType.Data
  | MkName            SubName.Data
  | MkExpire          SubExpire.Data
  | MkProviderPrivate SubProviderPrivate.Data
  | MkTitle           SubTitle.Data
