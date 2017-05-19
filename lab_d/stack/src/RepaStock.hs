{-# LANGUAGE FlexibleContexts #-}

module RepaStock where

import Prelude hiding (map)
import Data.Array.Repa
import Data.Array.Repa.Repr.HintInterleave
import Stock hiding (sell)

type SArray  = Array U DIM1 Day
type SRArray = Array U DIM1 Result

-- | Main solve function for stock market problem
buySell :: SArray -> IO Result
buySell arr = do
  results <- computeP $ hintInterleave $ map sell' arr :: IO SRArray
  foldAllP maxResult defaultRes results
    where
      s     = size $ extent arr
      sell' = sell arr s

-- | Given a buy day returns the optimal sell day and profit
sell :: SArray -> Int -> Day -> Result
sell arr s (i, b) = let (j, s) = foldAllS maxDay (0,0) sub in (i, j, s - b)
  where 
    sub = extract (Z :. i+1) (Z :. s-i-1) arr

-- | Parses a list of stock values into repa array representation
parse :: [Day] -> SArray
parse ins = let l = length ins in fromListUnboxed (Z :. (l::Int)) ins
