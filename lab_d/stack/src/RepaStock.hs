module RepaStock where

import Data.Array.Repa
import Stock

type SArray  = Array U DIM1 Int

-- | Main solve function for stock market problem
buySell :: Array U DIM1 Int -> Result
buySell = undefined

-- | Parses a list of stock values into repa array representation
parse :: [Int] -> SArray
parse ins = let l = length ins in fromListUnboxed (Z :. (l::Int)) ins
