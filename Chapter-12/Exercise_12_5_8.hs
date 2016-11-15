{-# OPTIONS_GHC -Wall #-}
module Exercise_12_5_8 where

type State = Int

newtype ST a = S (State -> (a, State))

extract :: ST a -> (State -> (a, State))
extract (S x) = x

instance Functor ST where
    fmap g st = do S func
                where func x = (g a, int)
                               where (a, int) = (extract st) x

instance Applicative ST where
    pure x = S (\s -> (x, s))
    sfx <*> stx = do S g
                  where g x = (h y, st)
                              where (y, st) = (extract stx) x
                                    (h, _ ) = (extract sfx) x

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Monad ST where
    st >>= f = S g
               where g s = app (f x) s'
                           where (x, s') = app st s