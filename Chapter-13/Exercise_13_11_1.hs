{-# OPTIONS_GHC -Wall #-}
module Exercise_13_11_1 where

import Control.Applicative

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

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x : xs) = do _ <- char x
                     _ <- string xs
                     return (x : xs)

comment :: Parser ()
comment = do _ <- string "--"
             _ <- many (sat (/= endl))
             _ <- char endl
             return ()
          where endl = '\n'