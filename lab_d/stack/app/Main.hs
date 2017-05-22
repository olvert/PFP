module Main where

import Stock as S
import RepaStock as R
import System.Random
import Criterion.Main
import Control.Monad

-- * Main functions

main :: IO ()
--main = standardTest
--main = repaTest
--main = repaMain
--main = standardMain
main = combined

combined :: IO ()
combined =
  let sXLarge = inputXLarge
      rXLarge = parse $ indices inputXLarge
  in defaultMain
      [ bench "stan - dyn x-large" $ nf S.buySellDyn sXLarge,
        bench "repa - dyn x-large" $ nfIO $ R.buySellDyn rXLarge
      ]
      

repaTest :: IO ()
repaTest = do
  let input = inputXLarge
  --r1 <- R.buySell $ parse $ indices input
  r2 <- R.buySellDyn $ parse $ indices input
  --print r1
  print r2
  
standardTest :: IO ()
standardTest = do
  let input = inputXLarge
  --print $ S.buySell input
  --print $ S.buySellDaQ input
  print $ S.buySellDyn input
  

repaMain :: IO ()
repaMain = 
  let inputSmall'  = parse $ indices inputSmall
      inputMedium' = parse $ indices inputMedium
      inputLarge' = parse $ indices inputLarge
  in defaultMain 
      [ bench "repa - small"      $ nfIO $ R.buySell inputSmall',
        bench "repa - medium"     $ nfIO $ R.buySell inputMedium',
        bench "repa - dyn small"  $ nfIO $ R.buySellDyn inputSmall',
        bench "repa - dyn medium" $ nfIO $ R.buySellDyn inputMedium',
        bench "repa - dyn large"  $ nfIO $ R.buySellDyn inputLarge']       
              
standardMain :: IO ()                     
standardMain = defaultMain 
                [ bench "stan - small"      $ nf S.buySell inputSmall,
                  bench "stan - medium"     $ nf S.buySell inputMedium,
                  bench "stan - daq small"  $ nf S.buySellDaQ inputSmall,
                  bench "stan - daq medium" $ nf S.buySellDaQ inputMedium,
                  bench "stan - dyn small"  $ nf S.buySellDyn inputSmall,
                  bench "stan - dyn medium" $ nf S.buySellDyn inputMedium,
                  bench "stan - dyn large"  $ nf S.buySellDyn inputLarge]
       

-- * Test related

inputSmall :: [Int]
inputSmall = [0,0,2,9,8,10,1,10]

inputMedium :: [Int]
inputMedium = generator seed $ 10^4

inputLarge :: [Int]
inputLarge = generator seed $ 10^5

inputXLarge :: [Int]
inputXLarge = generator seed $ 10^7

-- | The bounds for randomized stock value
bounds :: (Int, Int)
bounds = (0, 10000)

-- | Given a seed and length, generates a list of random stock values
generator :: StdGen -> Integer -> [Int]
generator seed 0 = []
generator seed n = 
  let (val, newSeed) = randomR bounds seed 
  in val : generator newSeed (n-1)

-- | Static seed
seed :: StdGen
seed = mkStdGen 2053754162 -- 2147483398
