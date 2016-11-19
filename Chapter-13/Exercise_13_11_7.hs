{-# OPTIONS_GHC -Wall #-}
module Exercise_13_11_7 where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
    fmap g p = P (\inp -> case parse p inp of
                               []         -> []
                               [(v, out)] -> [(g v, out)]
                               _          -> undefined)

instance Applicative Parser where
    pure v = P (\inp -> [(v, inp)])
    pg <*> px = P (\inp -> case parse pg inp of
                                []         -> []
                                [(g, out)] -> parse (fmap g px) out
                                _          -> undefined)

instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
                              []         -> []
                              [(v, out)] -> parse (f v) out
                              _          -> undefined)

instance Alternative Parser where
    empty   = P (\_ -> [])
    p <|> q = P (\inp -> case parse p inp of
                              []         -> parse q inp
                              [(v, out)] -> [(v, out)]
                              _          -> undefined)
    many x = some x <|> pure []
    some x = pure (:) <*> x <*> many x

item :: Parser Char
item = P (\inp -> case inp of
                       []        -> []
                       (x : xs)  -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x
               then return x
               else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x : xs) = do _ <- char x
                     _ <- string xs
                     return (x : xs)

space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do _ <- char '-'
         n <- nat
         return (-n)
         <|> nat

token :: Parser a -> Parser a
token p = do _ <- space
             v <- p
             _ <- space
             return v

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string

expr :: Parser Int
expr = do t <- term
          do _ <- symbol "+"
             e <- expr
             return (t + e)
             <|> do _ <- symbol "-"
                    e <- expr
                    return (t - e)
                    <|> return t

term :: Parser Int
term = do ep <- expo
          do _ <- symbol "*"
             t <- term
             return (ep * t)
             <|> do _ <- symbol "/"
                    t <- term
                    return (ep `div` t)
                    <|> return ep

expo :: Parser Int
expo = do f <- factor
          do _ <- symbol "^"
             e <- expo
             return (f ^ e)
             <|> return f

factor :: Parser Int
factor = do _ <- symbol "("
            e <- expr
            _ <- symbol ")"
            return e
            <|> integer

eval :: String -> Int
eval xs = case (parse expr xs) of
               [(n, [])]  -> n
               [(_, out)] -> error ("Unused input " ++ out)
               []         -> error "Invalid input"
               _          -> undefined