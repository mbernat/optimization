{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Driver where

import Data.RVar

import Problem
import Strategy

testDrive
    :: forall m s f a b.
    (MonadRandom m, RealFloat b, Strategy s)
    => Int
    -> s
    -> Problem f a b
    -> m (Result s f a b)
testDrive n s p = mIterate n init' step'
  where
    init' :: m (Result s f a b)
    init' = Right <$> initialState s p
    -- XXX this needs an ErrorT or something...
    step' :: Result s f a b -> m (Result s f a b)
    step' = \case
      Right x -> step s p x
      Left x -> pure $ Left x

mIterate :: Monad m => Int -> m a -> (a -> m a) -> m a
mIterate n x f = go n x
  where
    go 0 r = r
    go n' r = go (n' - 1) (r >>= f)


    
