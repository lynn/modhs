module Util where

onFst :: (a -> x) -> (a, b) -> (x, b)
onFst f (a, b) = (f a, b)

onSnd :: (b -> x) -> (a, b) -> (a, x)
onSnd f (a, b) = (a, f b)

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

addTowards :: Int -> Int -> Int -> Int
addTowards goal step x | x > goal + step = x - step
                       | x < goal - step = x + step
                       | otherwise       = goal
