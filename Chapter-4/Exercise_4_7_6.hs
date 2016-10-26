{-# OPTIONS_GHC -Wall #-}
module Exercise_4_7_6 where

import Prelude hiding ((&&))

(&&) :: Bool -> Bool -> Bool
x && y = if x
             then y
             else False