{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module RandomSearch
    ( RandomSearch(..)
    , RandomSearchState(..)
    , RandomSearchError(..)
    ) where

import Control.Monad
import Data.RVar

import BookKeeping
import Problem
import Strategy


data RandomSearch (g :: * -> *) = RandomSearch
    { samples :: Int
    }

data RandomSearchState f g a b = RandomSearchState
    { bestParams :: f a
    , bestValues :: g b
    }

deriving instance (Show (f a), Show (g b))
    => Show (RandomSearchState f g a b)

data RandomSearchError (f :: * -> *) (g :: * -> *) a b
    = RandomSearchError
  deriving (Show)


simpleSample
    :: (BookKeeping g, MonadRandom m, RealFloat b)
    => RandomSearch g
    -> Problem f a b
    -> m (f a, g b)
simpleSample RandomSearch{..} Problem{..} = do
    x  <- sampleRVar region
    fx <- fmap process
        . replicateM samples
        . sampleRVar
        $ objective x
    pure (x, fx)

instance BookKeeping g => Strategy (RandomSearch g) where
    type State (RandomSearch g) f a b = RandomSearchState f g a b
    type Error (RandomSearch g) f a b = RandomSearchError f g a b
    
    initialState rs p
        = uncurry RandomSearchState <$> simpleSample rs p
        
    step rs@RandomSearch{..} p best@RandomSearchState{..}
        = fmap Right $ do
            (y, fy) <- simpleSample rs p
            if summarize bestValues <=
               summarize fy
              then pure best
              else pure $ RandomSearchState y fy





    
