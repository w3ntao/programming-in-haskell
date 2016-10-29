{-# OPTIONS_GHC -Wall #-}
module Exercise_8_9_1 where

data Nat = Zero | Succ Nat
          deriving (Show)

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ i) n = Succ (add i n)

mult :: Nat -> Nat -> Nat
mult Zero     _ = Zero
mult (Succ i) n = add n (mult i n)