module Main where

import Stock
import RepaStock
import System.Random
import Criterion.Main
import Control.Monad

main :: IO ()
--main = standardTest
main = repaTest
--main = repaMain
--main = standardMain

repaTest :: IO ()
repaTest = do
  res <- RepaStock.buySell $ parse $ indices inputLarge
  print res
  
standardTest :: IO ()
standardTest = print $ Stock.buySell inputLarge

repaMain :: IO ()
repaMain = 
  let inputSmall'  = parse $ indices inputSmall
      inputMedium' = parse $ indices inputMedium
  in defaultMain 
      [ bench "repa - small" $ nfIO $ RepaStock.buySell inputSmall',
        bench "repa - medium" $ nfIO $ RepaStock.buySell inputMedium']       
              
standardMain :: IO ()                     
standardMain = defaultMain 
                [ bench "standard - small" $ nf Stock.buySell inputSmall,
                  bench "standard - medium" $ nf Stock.buySell inputMedium]
       

-- * Test related

inputSmall :: [Int]
inputSmall = [0,0,2,9,8,10,1,10]

inputMedium :: [Int]
inputMedium = generator seed 10000

inputLarge :: [Int]
inputLarge = generator seed 100000

-- | The bounds for randomized stock value
bounds :: (Int, Int)
bounds = (0, 10000)

-- | Given a seed and length, generates a list of random stock values
generator :: StdGen -> Int -> [Int]
generator seed 0 = []
generator seed n = 
  let (val, newSeed) = randomR bounds seed 
  in val : generator newSeed (n-1)

-- | Static seed
seed :: StdGen
seed = mkStdGen 2053754162 -- 2147483398
