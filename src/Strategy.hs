{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Strategy
    ( Result
    , Strategy(..)
    ) where

import Data.RVar

import Problem


type Result s f a b = Either (Error s f a b) (State s f a b)

class Strategy s where
    type State s (f :: * -> *) a b = r | r -> s f a b
    type Error s (f :: * -> *) a b = r | r -> s f a b

    initialState
        :: (MonadRandom m, RealFloat b)
        => s
        -> Problem f a b
        -> m (State s f a b)
    step
        :: (MonadRandom m, RealFloat b)
        => s
        -> Problem f a b
        -> State s f a b
        -> m (Result s f a b)
