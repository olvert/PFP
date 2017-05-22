{-# LANGUAGE FlexibleContexts #-}

module RepaStock where

import Prelude as P
import Data.Array.Repa as R
import Data.Array.Repa.Repr.HintInterleave
import Data.Vector.Unboxed as V

import Stock hiding (sell)

type SArray  = Array U DIM1 Day
type SRArray = Array U DIM1 Result

-- | 1: Parallel brute force solution with O(n^2)
buySell :: SArray -> IO Result
buySell arr = do
  results <- computeP $ hintInterleave $ R.map sell' arr :: IO SRArray
  foldAllP maxResult' defRes results
    where
      s     = size $ extent arr
      sell' = sell arr s

-- | Given a buy day returns the optimal sell day and profit
sell :: SArray -> Int -> Day -> Result
sell arr s (i, b) = let (j, s) = foldAllS maxDay defDay sub in (i, j, s - b)
  where 
    sub = extract (Z :. i+1) (Z :. s-i-1) arr
    

-- | 3: Parallel dynamic programming solution with O(n) (not so parallel though)
buySellDyn :: SArray -> IO Result
buySellDyn arr = foldAllP maxResult' defRes zipped
  where minBuy = fromUnboxed (Z :. (siz :: Int)) $ V.scanl1' minDay $ toUnboxed arr
        zipped = hintInterleave $ R.zipWith zipper minBuy arr
        zipper (i, b) (j, s) = (i, j, s - b)
        siz = size $ extent arr

-- | Parses a list of stock values into repa array representation
parse :: [Day] -> SArray
parse ins = let l = P.length ins in fromListUnboxed (Z :. (l::Int)) ins
