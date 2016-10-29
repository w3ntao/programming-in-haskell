{-# OPTIONS_GHC -Wall #-}
module Exercise_8_9_9 where

data Expr = Val Int
          | Add  Expr Expr
          | Mult Expr Expr

type Control = [Op]

data Op = EVAL Expr
        | ADD  Int
        | MULT Int

eval :: Expr -> Control -> Int
eval (Val n)    c = exec c n
eval (Add x y)  c = eval x (EVAL y : c)
eval (Mult x y) c = eval x (MULT (eval y []) : c)

exec :: Control -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD  n : c) m = exec c (n+m)
exec (MULT n : c) m = exec c (n*m)

value :: Expr -> Int
value e = eval e []