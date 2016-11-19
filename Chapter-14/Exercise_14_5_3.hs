{-# OPTIONS_GHC -Wall #-}
module Exercise_14_5_3 where

newtype Mmaybe a = M (Maybe a)

instance Functor Mmaybe where
    fmap _ (M Nothing)  = M Nothing
    fmap g (M (Just x)) = M (Just (g x))

instance Foldable Mmaybe where
    foldl _ v (M Nothing)  = v
    foldl f v (M (Just x)) = f v x

    foldr _ v (M Nothing)  = v
    foldr f v (M (Just x)) = f x v

    foldMap _ (M Nothing)  = mempty
    foldMap f (M (Just x)) = f x

instance Traversable Mmaybe where
    traverse g = sequenceA . fmap g