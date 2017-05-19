{-# LANGUAGE FlexibleContexts #-}

module RepaStock where

import Prelude hiding (map)
import Data.Array.Repa
import Stock

type SArray  = Array U DIM1 Day
type SRArray = Array U DIM1 Result

-- | Main solve function for stock market problem
buySell :: SArray -> IO Result
buySell arr = do
  results <- computeP $ map sell arr :: IO SRArray
  foldAllP local defaultRes results
    where
      local a b = if profit a > profit b then a else b
      s         = size $ extent arr
      sell      = sellR' arr s


sellR' :: SArray -> Int -> Day -> Result
sellR' arr s (i, b) = let (j, s) = foldAllS local (0,0) sub in (i, j, s - b)
  where
    sub       = extract (Z :. i+1) (Z :. s-i-1) arr
    local a b = if snd a >= snd b then a else b

sellR :: SArray -> Int -> Day -> Result
sellR arr s (i, b) = let (j, s) = local (i+1) (0,0) in (i, j, s - b) 
  where
    local j l | j >= s = l
    local j l = 
      let l' = arr ! (Z :. j) in 
      if snd l' > snd l then local (j+1) l' 
      else local (j+1) l

-- | Parses a list of stock values into repa array representation
parse :: [Day] -> SArray
parse ins = let l = length ins in fromListUnboxed (Z :. (l::Int)) ins
