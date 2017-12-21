{-

This is a simple CMA-ES strategy as described at
https://en.wikipedia.org/wiki/CMA-ES.

-}
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
    { lambda :: Int
      -- ^ Number of solutions to sample
    , weights :: [Double]
      -- ^ The recombination weights should:
      --  be decreasing
      --  sum to one
      --  have length <= lambda/2
      --  their muW = inverted sum of squares of weights ~ lambda/4
    , csigma :: Double
      -- ^ The backward time horizon for the evolution pathSigma.
      -- Should be ~ 3/n and larger than 1.
    , dsigma :: Double
      -- ^ The damping parameter, usually close to one.
    , cc :: Double
      -- ^ The backward time horizon for the evolution pathC.
      -- Should be ~ 4/n and larger than 1.
    , alpha :: Double
      -- ^ The cut-off for pathSigma norm. Should be ~ 1.5.
    , c1 :: Double
      -- ^ The learning rate for the rank-one update of the covariance
      -- matrix. Should be ~ 2/(n^2)
    , cmu :: Double
      -- ^ The learning rate for the rank-mu update of the covariance
      -- matrix and must not exceed 1 - c1.
      -- Should be ~ muW/n^2.
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

square x = x*x

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
        samples <- replicateM lambda $ do
            x <- sampleRVar $ normal mean (scale (sigma*sigma) cov)
            -- Is this meaningful for stochastic functions?
            y <- sampleRVar $ objective x
            pure (x, y)
        let n = size $ head samples
        let ordered = fmap fst $ sortBy (comparing snd) samples

        let displace x = scale (1/sigma) (x - mean)
        let mean' = updateMean ordered
        let muW = (1 /) . sum $ map square weights

        let normPathSigma = norm_2 pathSigma
        let muDisp = scale (sqrt muW) (displace mean')
        let muDispNorm = sqrtm (inv cov) #> muDisp
        let pathSigma' = updatePathSigma n pathSigma muDispNorm
        let pathC' = updatePathC pathC muDisp normPathSigma
        let cov' = updateCov cov pathC (fmap displace ordered)
        let sigma' = updateSigma n sigma normPathSigma

        pure . Right $ State
            { mean = mean'
            , sigma = sigma'
            , cov = cov'
            , pathSigma = pathSigma'
            , pathC = pathC'
            }
      where
        updateMean samples'
           = sum $ zipWith scale weights samples'

        updatePathSigma n pathSigma' muDispNorm
           = scale (1 - csigma) pathSigma' + scale discVarComp muDisp
          where
            discVarComp = sqrt $ 1 - square (1 - csigma)

        updatePathC pathC' muDisp npsigma
            = scale (1 - cc) pathC' + scale (ind * discVarComp) muDisp
          where
            discVarComp = sqrt $ 1 - square (1 - cc)

        updateCov cov' _pathC' _diffs = cov'

        updateSigma n sigma' npsigma'
            = sigma' * exp ((csigma / dsigma) * (npsigma' / expVal - 1))
          where
            -- E |N(0, ident)|
            expVal = (sqrt n) * (1 - 1/(4*n) + (1/(21*n*n)))
