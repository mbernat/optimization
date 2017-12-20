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

data State (f :: * -> *) a b = State
    { mean :: Vector Double
    , sigma :: Double
    , cov :: Matrix Double
    , pathSigma :: Vector Double
    , pathC :: Vector Double
    }

data Error (f :: * -> *) a b = Error

-- TODO implement multivariate normal distribution
normal :: Vector Double -> Matrix Double -> RVar (Vector Double)
normal mean _cov = pure mean

instance Strategy Cmaes where
    type State Cmaes f a b = State f a b
    type Error Cmaes f a b = Error f a b

    -- TODO what's a good way to initialize mean and sigma?
    initialState _s _p
        = pure $ State
            { mean = zero
            , sigma = 1
            , cov = ident (size zero)
            , pathSigma = zero
            , pathC = zero
            }
          where
            zero = 0

    step Cmaes{..} Problem{..} State{..} = do
        samples <- replicateM numSamples $ do
            x <- sampleRVar $ normal mean (scale (sigma*sigma) cov)
            -- XXX we need to produce (f a) from Vector Double ... looks like the interface is way too generic...
            x' <- sampleRVar region
            y <- sampleRVar $ objective x'
            pure (x, y)
        -- TODO handle the case sigma == 0 by producing an error
        let sorted = fmap fst $ sortBy (comparing snd) samples
        let mean' = updateMean mean sorted
        let pathSigma' = updatePathSigma pathSigma (scale (1/sigma) $ (sqrtm (inv cov)) #> (mean' - mean))
        let pathC' = updatePathC pathC (scale (1/sigma) (mean' - mean)) (norm_2 pathSigma)
        let cov' = updateCov cov pathC (fmap (\x -> scale (1/sigma) (x - mean')) sorted)
        let sigma' = updateSigma sigma (norm_2 pathSigma)
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
