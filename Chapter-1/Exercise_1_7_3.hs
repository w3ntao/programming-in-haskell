{-# OPTIONS_GHC -Wall #-}
module Exercise_1_7_3 where

import Prelude hiding (product)

product :: Num t => [t] -> t
product []       = 1
product (x : xs) = x * product xs