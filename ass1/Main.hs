module Main where

import Data.List hiding (partition)
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies as Strategies
import Control.DeepSeq
import Control.Monad.Par as Par
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


-- * Parallell Map (Assignment 1)

-- | Parallel map utilising par with depth
parMap :: (NFData b) => Int -> ([a] -> b) -> [[a]] -> [b]
parMap d f [] = []
parMap 0 f as = map f as
parMap d f as = par (rnf ys') (xs' ++ ys')
  where (xs, ys) = splitInHalf as
        xs'      = Main.parMap (d-1) f xs
        ys'      = Main.parMap (d-1) f ys

-- | Parallel map utilising rpar with depth
parMapRD :: (NFData b) => Int -> ([a] -> b) -> [[a]] -> [b]
parMapRD d f [] = []
parMapRD 0 f as = map f as
parMapRD d f as = runEval $ do
  let (xs, ys) = splitInHalf as
  xs' <- rpar $ force $ parMapRD (d-1) f xs
  ys' <- rpar $ force $ parMapRD (d-1) f ys
  return $ xs' ++ ys'

-- | Parallell map utilising Strategies
parMapS :: (NFData b) => ([a] -> b) -> [[a]] -> [b]
parMapS = Strategies.parMap rdeepseq

-- | Parallell map utilising the Par monad
parMapP :: (NFData b) => (a -> b) -> [a] -> [b]
parMapP f as = runPar $ Par.parMap f as

-- | Parallell map utilising the Par monad with depth
parMapPD :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMapPD d f [] = []
parMapPD 0 f as = map f as
parMapPD d f as = runPar $ do
  let (xs, ys) = splitInHalf as
  xs' <- new
  ys' <- new
  fork $ put xs' (parMapPD (d-1) f xs)
  fork $ put ys' (parMapPD (d-1) f ys)
  xs'' <- get xs'
  ys'' <- get ys'
  return $ xs'' ++ ys''


-- * Parallell Merge (Assignment 2)

-- | Merge two lists recursively
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
         | x <= y  = x:merge xs (y:ys)
         | otherwise = y:merge (x:xs) ys

-- | Basic mergesort
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (a, b) = splitInHalf xs
               in merge (mergesort a) (mergesort b)

-- | Parallel mergesort utilising rpar with depth
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
jackknife f = map f . resamples 500
-- jackknife f = Main.parMap 2 f . resamples 500
-- jackknife f = parMapRD 3 f . resamples 500
-- jackknife f = parMapS f . resamples 500
-- jackknife f = parMapP f . resamples 500
-- jackknife f = parMapPD 4 f . resamples 500

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
-- main = sortBench
main = jackBench
