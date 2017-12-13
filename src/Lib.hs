{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( Options(..)
    , Result
    , randomSearch
    ) where

import Control.Monad
import Data.List
import Data.RVar
import Data.Random
import Data.Random.Distribution.Uniform

type ArgVal f a b = (a, f b)
type Result f a b = ArgVal f a b

newtype Pair a = Pair { getPair :: (a, a) }

data Options f a = Options
    { input :: RVar a
    , maxIter :: Int
    , maxSamples :: Int
    -- TODO make this abstract
    , process :: forall b. RealFloat b => [b] -> f b
    , summarize :: forall b. RealFloat b => f b -> b
    }

listOptions :: Options [] Double
listOptions = Options stdUniform 50 50 sort mean

statOptions :: Options Pair Double
statOptions = Options stdUniform 50 50 meanVar (fst . getPair)

mean xs = sum xs / (fromIntegral $ length xs)

meanVar
    :: Floating a
    => [a]
    -> Pair a
meanVar xs = Pair (avg, sqrt var)
  where
    avg = mean xs
    var = sum (fmap diffSq xs) / realToFrac (length xs - 1)
    diffSq x = (x - avg) ^ (2::Int)
    len = fromIntegral $ length xs

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
    process' = fmap process . replicateM maxSamples . sampleRVar . f
    avg xs  = sum xs / (fromIntegral $ length xs)
    go :: Result f a b -> Int -> m (Result f a b)
    go best@(x, fx) = \case
        0 -> pure best
        n -> do
            y <- sampleRVar input
            fy <- process' x
            let next = if summarize fx <= summarize fy then best else (y, fy)
            go next (n-1)
    
