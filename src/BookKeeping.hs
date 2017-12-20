module BookKeeping
    ( BookKeeping(..)
    , Pair(..)
    ) where

import Data.List


newtype Pair a = Pair { getPair :: (a, a) }
  deriving (Show)

class BookKeeping f where
    process :: RealFloat b => [b] -> f b
    summarize :: RealFloat b => f b -> b

instance BookKeeping [] where
    process = sort
    summarize = head

instance BookKeeping Pair where
    process = meanVar
    summarize = fst . getPair


mean :: (Foldable f, Fractional a, Ord a) => f a -> a
mean xs
    | len > 0 = sum xs / len
    | otherwise = error "Cannot compute mean of an empty structure"
  where
    len = fromIntegral $ length xs

meanVar
    :: (Foldable f, Functor f, Floating a, Ord a)
    => f a
    -> Pair a
meanVar xs
    | length xs >= 2 = Pair (avg, sqrt var)
    | otherwise = error "Cannot compute variance of fewer than two points."
  where
    avg = mean xs
    var = sum (fmap diffSq xs) / realToFrac (length xs - 1)
    diffSq x = (x - avg) ^ (2::Int)
