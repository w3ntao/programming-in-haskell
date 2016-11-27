{-# OPTIONS_GHC -Wall #-}
module Exercise_4_8_4 where

import Prelude hiding ((||))

(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True