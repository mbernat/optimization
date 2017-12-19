{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( Options(..)
    , Pair(..)
    , Result
    , listOptions
    , randomSearch
    , statOptions
    ) where

import Control.Monad
import Data.List
import Data.RVar
import Data.Random


type ArgVal f a b = (a, f b)
type Result f a b = ArgVal f a b

newtype Pair a = Pair { getPair :: (a, a) }
  deriving (Show)

data BookKeeping f where
    List :: BookKeeping []
    Stat :: BookKeeping Pair

instance Show (BookKeeping f) where
    show _ = "BookKeeping"

instance Show (RVar a) where
    show _ = "RVar"

data Options f a = Options
    -- TODO rvar is not really showable...
    { input :: RVar a
    , maxIter :: Int
    , maxSamples :: Int
    , bookKeeping :: BookKeeping f
    }
  deriving (Show)

process :: RealFloat b => BookKeeping f -> [b] -> f b
process = \case
    List -> sort
    Stat -> meanVar

summarize :: RealFloat b => BookKeeping f -> f b -> b
summarize = \case
    List -> head
    Stat -> (fst . getPair)

listOptions :: Options [] Double
listOptions = Options stdUniform 50 50 List

statOptions :: Options Pair Double
statOptions = Options stdUniform 50 50 Stat

mean :: (Foldable f, Fractional a, Ord a) => f a -> a
mean xs
    | len > 0 = sum xs / len
    | otherwise = error "Cannot compute the mean of an empty structure"
  where
    len = fromIntegral $ length xs

meanVar
    :: (Foldable f, Functor f, Floating a, Ord a)
    => f a
    -> Pair a
meanVar xs
    | length xs >= 2 = Pair (avg, sqrt var)
    | otherwise = error "Cannot compute the variance of less than two points."
  where
    avg = mean xs
    var = sum (fmap diffSq xs) / realToFrac (length xs - 1)
    diffSq x = (x - avg) ^ (2::Int)

randomSearch
    :: forall a b f m. (RealFloat b, MonadRandom m)
    => (a -> RVar b)
    -> Options f a
    -> m (Result f a b)
randomSearch f Options{..} = do
    x  <- sampleRVar input
    fx <- process' x
    go (x, fx) (maxIter - 1)
  where
    process' = fmap (process bookKeeping) . replicateM maxSamples . sampleRVar . f
    summarize' = summarize bookKeeping
    go :: Result f a b -> Int -> m (Result f a b)
    -- Here best is the internal state.
    go best@(_, fx) = \case
        -- Project the internal into the final result.
        0 -> pure best
        n -> do
            -- Sample
            y <- sampleRVar input
            fy <- process' y
            let next = if summarize' fx <= summarize' fy then best else (y, fy)
            go next (n-1)



{-

Let's generalize the random search so that it's just a particular
instance of an optimization procedure.  Such a procedure would carry a
state representing the knowledge about the objective function and be
allowed to do two things: a) sample the function to obtain new
information; and b) once it has enough information update its state;
and c) optionally terminate.  Finally there should be a projection
from the final internal state to the optimal paramaters, values or
other useful data.

-}
