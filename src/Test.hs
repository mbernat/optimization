{-# LANGUAGE RecordWildCards #-}
module Test where

import Data.Functor.Identity
import Data.Random
import Numeric.LinearAlgebra

import BookKeeping
import Cmaes
import Driver
import Problem
import Strategy
import RandomSearch


problem :: Problem Identity Double Double
problem = Problem
    { objective = pure . cos . runIdentity
    , region = Identity <$> stdUniform
    }

randomDrive :: IO (Result (RandomSearch Pair) Identity Double Double)
randomDrive = testDrive iter RandomSearch{..} problem
  where
    samples = 10
    iter = 100

problem2 :: Problem Vector Double Double
problem2 = Problem
    { objective = pure . cos . norm_2
    , region    = pure $ fromList [0, 0]
    }
  
cmaesDrive :: IO (Result Cmaes Vector Double Double)
cmaesDrive = testDrive iter Cmaes{..} problem2
  where
    lambda = 10
    weights = [0.4, 0.3, 0.2, 0.1]
    iter = 100
