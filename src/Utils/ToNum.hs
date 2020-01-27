module Utils.ToNum where

import Common

class Class a where
  to_num :: (Num b) => a -> b
