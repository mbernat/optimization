module Problem
    ( Problem(..)
    ) where

import Data.RVar


data Problem f a b = Problem
    { objective :: f a -> RVar b
    , region    :: RVar (f a)
    }
