module Descriptor.LangCode where

type Data_ISO_639 = (Char,Char,Char)
type Data = Data_ISO_639

class Has a where
  lang_code :: a -> Data
