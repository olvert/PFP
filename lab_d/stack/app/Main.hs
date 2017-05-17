module Main where

import Stock
import System.Random
import Criterion.Main

main :: IO ()
-- main = print $ Stock.buySell inputSmall
main = defaultMain [ bench "small" $ nf Stock.buySell inputSmall,
                     bench "medium" $ nf Stock.buySell inputMedium]


-- * Test related

inputSmall :: [Int]
inputSmall = [0,0,2,9,8,10,1,10]

inputMedium :: [Int]
inputMedium = generator seed 10000

-- | The bounds for randomized stock value
bounds :: (Int, Int)
bounds = (0, 1000)

-- | Given a seed and length, generates a list of random stock values
generator :: StdGen -> Int -> [Int]
generator seed 0 = []
generator seed n = 
  let (val, newSeed) = randomR bounds seed 
  in val : generator newSeed (n-1)

-- | Static seed
seed :: StdGen
seed = mkStdGen 2053754162 -- 2147483398
