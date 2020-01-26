module Descriptor.CarouselCompatibleComposite.SubDescriptor.Base where

import Utils
import qualified Parser.Result as Result
import qualified Descriptor.Header as Header

class (Header.Class a) => Class a where
