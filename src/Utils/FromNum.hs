module Utils.FromNum where

import Common

class Class a where
  from_num :: (Integral b) => b -> a 
