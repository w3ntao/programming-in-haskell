{-# OPTIONS_GHC -Wall #-}
module Exercise_11_13_3 where

import Data.List
import System.IO
import Exercise_11_13_2 hiding (validMoves, randomPick, play, play', main)

fastestMove :: Grid -> Player -> Grid
fastestMove g p = head [g' | Node (g', p') _ <- sortOn treeDepth ts, p' == best]
                  where tree = prune depth (gameTree g p)
                        Node (_, best) ts = minimax tree

treeDepth :: Tree a -> Int
treeDepth (Node _ []) = 0
treeDepth (Node _ ts) = 1 + minimum (map treeDepth ts)

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
                           play (fastestMove g p) (nextPlayer p)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O