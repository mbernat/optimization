module Problem
    ( Problem(..)
    ) where

import Data.RVar

-- We should be able to accomodate many different classes of problems,
-- including:
--  stochastic/non-stochastic objective functions
--  make it explicit when the objective function is convex or has
--   nice other properties that are only exploitable by certain
--   strategies
--  (in)equality constraints

data Problem f a b = Problem
    { objective :: f a -> RVar b
    , region    :: RVar (f a)
    }
