{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module RandomSearch
    ( RandomSearch(..)
    , State(..)
    , Error(..)
    ) where

import Control.Monad
import Data.RVar

import BookKeeping
import Problem
import Strategy (Strategy)
import qualified Strategy


data RandomSearch (g :: * -> *) = RandomSearch
    { samples :: Int
    }

data State f g a b = State
    { bestParams :: f a
    , bestValues :: g b
    }

deriving instance (Show (f a), Show (g b))
    => Show (State f g a b)

data Error (f :: * -> *) (g :: * -> *) a b = Error
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

instance BookKeeping g => Strategy (RandomSearch g) f a b where
    type State (RandomSearch g) f a b = State f g a b
    type Error (RandomSearch g) f a b = Error f g a b
    
    initialState rs p
        = uncurry State <$> simpleSample rs p
        
    step rs@RandomSearch{..} p best@State{..}
        = fmap Right $ do
            (y, fy) <- simpleSample rs p
            if summarize bestValues <= summarize fy
                then pure best
                else pure $ State y fy





    
