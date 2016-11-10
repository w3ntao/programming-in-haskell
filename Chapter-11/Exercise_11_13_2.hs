{-# OPTIONS_GHC -Wall #-}
module Exercise_11_13_2 where

import Data.Char
import Data.List
import System.IO
import System.Random

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer B = B
nextPlayer X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where os = length (filter (== O) ps)
               xs = length (filter (== X) ps)
               ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where line = all (== p)
                 rows = g
                 cols = transpose g
                 dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate (size*4 - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where beside = foldr1 (zipWith (++))
                bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer p = ["   ", sp p , "   "]
               where sp O = " O "
                     sp B = "   "
                     sp X = " X "

interleave :: a -> [a] -> [a]
interleave _ []       = []
interleave _ [y]      = [y]
interleave x (y : ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^(2::Int) && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i
                 then [chop size (xs ++ [p] ++ ys)]
                 else []
             where (xs, B : ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs
                       then return (read xs)
                       else
                           do putStrLn "ERROR: Invalid number"
                              getNat prompt

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
              deriving Show

gameTree :: Grid -> Player -> Tree Grid
gameTree g p = Node g [gameTree g' (nextPlayer p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won  g    = []
          | full g    = []
          | otherwise = concat [move g i p | i <- [0..(size^(2::Int))-1]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _ ) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g []) | wins O g  = Node (g, O) []
                    | wins X g  = Node (g, X) []
                    | otherwise = Node (g, B) []

minimax (Node g ts) | turn g == O = Node (g, minimum ps) ts'
                    | turn g == X = Node (g, maximum ps) ts'
                                    where ts' = map minimax ts
                                          ps  = [p | Node (_, p) _ <- ts']

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1, 1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p | wins O g  = putStrLn "Player O wins!\n"
          | wins X g  = putStrLn "Player X wins!\n"
          | full g    = putStrLn "It's a draw!\n"
          | p == O    = do i <- getNat (prompt p)
                           case move g i p of
                                []   -> do putStrLn "ERROR: Invalid move"
                                           play' g p
                                [g'] -> play g' (nextPlayer p)
          | p == X    = do putStr "Player X is thinking... "
                           rMove <- randomPick (validMoves g)
                           play (head (move g rMove p)) (nextPlayer p)

validMoves :: Grid -> [Int]
validMoves g = [m | m <-[0..size^(2::Int)-1], valid g m]

randomPick :: [a] -> IO a
randomPick xs = do randNum <- randomRIO (0, (length xs)-1)
                   return (xs !! randNum)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O