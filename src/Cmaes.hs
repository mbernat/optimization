{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Cmaes where

import Control.Monad
import Data.List
import Data.Ord
import Data.RVar
import Numeric.LinearAlgebra

import Problem
import Strategy (Strategy)
import qualified Strategy


data Cmaes = Cmaes
    { numSamples :: Int
    }

data State = State
    { mean :: Vector Double
    , sigma :: Double
    , cov :: Matrix Double
    , pathSigma :: Vector Double
    , pathC :: Vector Double
    }
  deriving (Show)

data Error = Error
  deriving (Show)

-- TODO implement multivariate normal distribution
normal :: Vector Double -> Matrix Double -> RVar (Vector Double)
normal mean _cov = pure mean

instance Strategy Cmaes Vector Double Double where
    type State Cmaes Vector Double Double = State
    type Error Cmaes Vector Double Double = Error

    -- TODO what's a good way to initialize mean and sigma?
    initialState _s p = do
        initial <- sampleRVar $ region p
        let zero = cmap (const 0) initial
        pure $ State
            { mean = initial
            , sigma = 1
            , cov = ident (size zero)
            , pathSigma = zero
            , pathC = zero
            }


    step Cmaes{..} Problem{..} State{..} = do
        samples <- replicateM numSamples $ do
            x <- sampleRVar $ normal mean (scale (sigma*sigma) cov)
            y <- sampleRVar $ objective x
            pure (x, y)
        let sorted = fmap fst $ sortBy (comparing snd) samples

        let mean' = updateMean mean sorted
        let meanDiff = mean' - mean
        let compDiff x = scale (1/sigma) (x - mean')

        let normPathSigma = norm_2 pathSigma
        let pathSigma' = updatePathSigma pathSigma (scale (1/sigma) $ (sqrtm (inv cov)) #> meanDiff)
        let pathC' = updatePathC pathC (scale (1/sigma) meanDiff) normPathSigma
        let cov' = updateCov cov pathC (fmap compDiff sorted)
        let sigma' = updateSigma sigma normPathSigma
        pure . Right $ State
            { mean = mean'
            , sigma = sigma'
            , cov = cov'
            , pathSigma = pathSigma'
            , pathC = pathC'
            }
      where
        updateMean mean' _samples' = mean'
        updatePathSigma pathSigma' _diff = pathSigma'
        updatePathC pathC' _diff _npsigma = pathC'
        updateCov cov' _pathC' _diffs = cov'
        updateSigma sigma' _npsigma = sigma'
