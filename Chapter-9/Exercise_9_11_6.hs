{-# OPTIONS_GHC -Wall #-}
module Exercise_9_11_6 where

import Data.List
import Exercise_9_11_1

data Op = Add | Sub | Mul | Div | Exp

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

valid :: Op -> Integer -> Integer -> Bool
valid Add x y = x <= y
valid Sub x y = x >  y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = x /= 1 && y /= 1

apply :: Op -> Integer -> Integer -> Integer
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div
apply Exp = (^)

data Expr = Val Integer | App Op Expr Expr

instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where brak (Val n) = show n
                             brak e       = "(" ++ show e ++ ")"

split :: [a] -> [([a], [a])]
split []       = []
split [_]      = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

type Result = (Expr, Integer)

results :: [Integer] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                      lx      <- results ls,
                      ry      <- results rs,
                      res     <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Integer] -> Integer -> [Expr]
solutions' ns n = [e |  ns'   <- choices ns,
                       (e, m) <- results ns',
                        m == n]

firstSolution :: [Integer] -> Integer -> (Maybe Expr)
firstSolution ns n = if not (null answer)
                         then Just (head answer)
                         else Nothing
                     where answer = solutions' ns n

simplicity :: Expr -> Integer
simplicity (Val _)     = 1
simplicity (App o x y) = opSimp o + simplicity x + simplicity y
                         where opSimp Add = 2
                               opSimp Sub = 2
                               opSimp Mul = 4
                               opSimp Div = 4
                               opSimp Exp = 5

sortedSolutions :: [Integer] -> Integer -> [Expr]
sortedSolutions ns n = sortOn simplicity (solutions' ns n)