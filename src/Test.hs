module Test where

import Data.Functor.Identity
import Data.Random

import BookKeeping
import Driver
import Problem
import Strategy
import RandomSearch


problem :: Problem Identity Double Double
problem = Problem
    { objective = pure . cos . runIdentity
    , region = Identity <$> stdUniform
    }

drive :: IO (Result (RandomSearch Pair) Identity Double Double)
drive = testDrive iter (RandomSearch numSamples) problem
  where
    numSamples = 10
    iter = 100
  
