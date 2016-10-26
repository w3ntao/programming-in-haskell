{-# OPTIONS_GHC -Wall #-}
module Exercise_4_7_5 where

import Prelude hiding ((&&))

(&&) :: Bool -> Bool -> Bool
x && y = if x
             then if y
                 then True
                 else False
             else False