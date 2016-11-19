{-# OPTIONS_GHC -Wall #-}
module Exercise_14_5_5 where

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldl (\xs x -> (if f x then [x] else []) ++ xs ) []