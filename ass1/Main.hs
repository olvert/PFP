module Main where

import Data.List hiding (partition)
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Utils

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion


-- * Predefined

data T a = T !a !Int

mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1

resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]


-- * Parallell Map

-- | Parallel map function running each element in parallel
-- | Status: Broken because of lazy evaluation
mapPseq :: ([a] -> b) -> [[a]] -> [b]
mapPseq f []     = []
mapPseq f (a:as) = par b (pseq bs (b:bs))
  where b  = f a
        bs = mapPseq f as

-- | Parallel map function using depth to recursively split list into sublists
-- | and run the evaluation in parallel (DaC = Divide and Conquer)
-- | Status: Works by forcing evaluation by 'rnf'
mapDaC :: (NFData b) => Int -> ([a] -> b) -> [[a]] -> [b]
mapDaC d f [] = []
mapDaC 0 f as = map f as
mapDaC d f as = par (rnf ys') (xs' ++ ys')
  where (xs, ys) = splitInHalf as
        xs'      = mapDaC (d-1) f xs
        ys'      = mapDaC (d-1) f ys

-- | Parallel map function that runs map in parallel over sublists
-- Status: Broken because of lazy evaluation
mapPseqP :: Int -> ([a] -> b) -> [[a]] -> [b]
mapPseqP s f as = pseq as' (concat as')
  where as' = mapPseq (map f) $ partition s as

parMapD :: (NFData b) => Int -> ([a] -> b) -> [[a]] -> [b]
parMapD d f [] = []
parMapD 0 f as = map f as
parMapD d f as = runEval $ do
  let (xs, ys) = splitInHalf as
  --    xs'      = parMapD (d-1) f xs
  xs' <- rpar $ force $ parMapD (d-1) f xs
  ys' <- rpar $ force $ parMapD (d-1) f ys
  --rseq xs'
  --rseq ys'
  return $ xs'++ys'


-- * Parallell Merge

-- | merge recursively two lists
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
         | x <= y  = x:merge xs (y:ys)
         | otherwise = y:merge (x:xs) ys

-- | mergesort implemntation
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (a, b) = splitInHalf xs
               in merge (mergesort a) (mergesort b)

-- | With d granularity and rpar
mergesortD :: (NFData a, Ord a) => Int -> [a] -> [a]
mergesortD _ []  = []
mergesortD _ [x] = [x]
mergesortD 0 as = mergesort as
mergesortD d as = runEval $ do
  let (xs, ys) = splitInHalf as
  xs' <- rpar $ force $ mergesortD (d-1) xs
  ys' <- rpar $ force $ mergesortD (d-1) ys
  return $ merge xs' ys'


-- * Benchmark Related

-- | Placeholder for current map strategy
jackknife :: (NFData b) => ([a] -> b) -> [a] -> [b]
jackknife f = mapPseq f . resamples 500
-- jackknife f = parMapD 3 f . resamples 500
-- jackknife f = mapDaC 2 f . resamples 500

-- | Placeholder for current sort strategy
sortfun :: (NFData a, Ord a) => [a] -> [a]
--sortfun = mergesortD 2
sortfun = mergesortD 1


-- | Main function for sort benchmark
sortBench :: IO ()
sortBench = do
  let rnds = take 6000 (randoms (mkStdGen 211570155)) :: [Int]
  defaultMain [ bench "sort" $ nf sortfun rnds ]
  return ()

-- | Main function for map benchmark
jackBench :: IO ()
jackBench = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  defaultMain
        [
         bench "jackknife" (nf (jackknife  mean) rs)
         ]

main :: IO ()
main = sortBench
--main = jackBench
