module Main where

import Data.List hiding (partition)
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies as Strategies
import Control.DeepSeq
import Control.Monad
import Control.Monad.Par as Par
import Utils

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion


-- * Type Signatures

-- | Used to reduce length of type signatures slightly
type TJoin b = [b] -> [b] -> [b]

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
parMap :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMap d f [] = []
parMap 0 f as = map f as
parMap d f as = par (rnf ys') (xs' ++ ys')
  where (xs, ys) = splitInHalf as
        xs'      = Main.parMap (d-1) f xs
        ys'      = Main.parMap (d-1) f ys
        
-- | Parallel map utilising par with depth
parMap' :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMap' d f as = fun d (mdl as) f as
  where
    --fun :: Int -> Int -> (a -> b) -> [a] -> [b]
    fun d i f [] = []
    fun 0 i f as = map f as
    fun d i f as = par (rnf ys') (xs' ++ ys')
      where (xs, ys) = splitAt i as
            eval     = fun (d-1) (i `div` 2) f
            xs'      = eval xs
            ys'      = eval ys

-- | Parallel map utilising rpar with depth
parMapRD :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMapRD d f [] = []
parMapRD 0 f as = map f as
parMapRD d f as = runEval $ do
  let (xs, ys) = splitInHalf as
  xs' <- rpar $ force $ parMapRD (d-1) f xs
  ys' <- rpar $ force $ parMapRD (d-1) f ys
  return $ xs' ++ ys'
  
  
-- | Parallel map utilising rpar with depth
parMapRD' :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMapRD' d f as = runEval $ fun d (mdl as) f as
  where 
    fun :: (NFData b) => Int -> Int -> (a -> b) -> [a] -> Eval [b]
    fun d i f [] = return []
    fun 0 i f as = return $ force $ map f as
    fun d i f as = do
      let (xs, ys) = splitAt i as
      let eval as  = fmap (rpar . force) (fun (d-1) (i `div` 2) f as)
      ys' <- eval ys
      xs' <- eval xs
      liftM2 (++) xs' ys'

-- | Parallell map utilising Strategies
parMapS :: (NFData b) => (a -> b) -> [a] -> [b]
parMapS = Strategies.parMap rdeepseq

-- | Parallell map utilising the Par monad
parMapP :: (NFData b) => (a -> b) -> [a] -> [b]
parMapP f as = runPar $ Par.parMap f as

-- | Parallell map utilising the Par monad with depth
parMapPD :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMapPD d f [] = []
parMapPD 0 f as = map f as
parMapPD d f as = runPar $ dacPar (parMapPD (d-1) f) (++) as

-- | Parallell map utilising the Par monad with depth
parMapPD' :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMapPD' d f as = runPar $ fun f d (mdl as) as
  where
    fun :: (NFData b) => (a -> b) -> Int -> Int -> [a] -> Par [b]
    fun f d i [] = return []
    fun f 0 i as = return $ map f as
    fun f d i as = dacPar' (fun f (d-1) j) (++) xs ys
      where 
        (xs, ys) = splitAt i as
        j        = i `div` 2

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
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = let (a, b) = splitInHalf xs
                in merge (mergesort a) (mergesort b)
               
               
-- | Basic mergesort
mergesort' :: Ord a => Int -> [a] -> [a]
mergesort' i []  = []
mergesort' i [x] = [x]
mergesort' i xs  = let (a, b) = splitAt i xs
                       j      = i `div` 2
                   in merge (mergesort' j a) (mergesort' j b)

-- | Parallel mergesort utilising par and pseq
mergesortPseq :: (NFData a, Ord a) => [a] -> [a]
mergesortPseq [] = []
mergesortPseq [x] = [x]
mergesortPseq xs = dacPseq mergesortPseq merge xs

-- | Parallel mergesort utilising rpar with depth
mergesortRD :: (NFData a, Ord a) => Int -> [a] -> [a]
mergesortRD _ []  = []
mergesortRD _ [x] = [x]
mergesortRD 0 as = mergesort as
mergesortRD d as = runEval $ do
  let (xs, ys) = splitInHalf as
  xs' <- rpar $ force $ mergesortRD (d-1) xs
  ys' <- rpar $ force $ mergesortRD (d-1) ys
  return $ merge xs' ys'

-- | Parallel mergesort utilising rpar with depth
mergesortRD' :: (NFData a, Ord a) => Int -> [a] -> [a]
mergesortRD' d as  = runEval $ fun d (mdl as) as
  where 
    fun d i []  = return []
    fun d i [x] = return [x]
    fun 0 i as  = return $ mergesort' (i `div` 2) as
    fun d i as  = do
      let (xs, ys) = splitAt i as
      let eval as  = join $ fmap (rpar . force) (fun (d-1) (i `div` 2) as)
      xs' <- eval xs
      ys' <- eval ys
      return $ merge xs' ys'

-- | Parallel mergesort utilising the Par monad
mergesortP :: (NFData a, Ord a) => [a] -> [a]
mergesortP []  = []
mergesortP [x] = [x]
mergesortP as  = runPar $ dacPar mergesortP merge as

-- | Parallel mergesort utilising the Par monad with depth
mergesortPD :: (NFData a, Ord a) => Int -> [a] -> [a]
mergesortPD d []  = []
mergesortPD d [x] = [x]
mergesortPD 0 as  = mergesort as
mergesortPD d as  = runPar $ dacPar (mergesortPD (d-1)) merge as

-- | Parallel mergesort utilising the Par monad with depth
mergesortPD' :: (NFData a, Ord a) => Int -> [a] -> [a]
mergesortPD' d as = runPar $ fun d (mdl as) as
  where
    fun d i []  = return []
    fun d i [x] = return [x]
    fun 0 i as  = return $ mergesort as
    fun d i as  = dacPar' (fun (d-1) j) merge xs ys
      where 
        (xs, ys) = splitAt i as
        j        = i `div` 2
        

-- * Helpers

-- | Helper function encapsulating the DaC pattern with the Par monad
dacPar :: (NFData b) => ([a] -> [b]) -> TJoin b -> [a] -> Par [b]
dacPar f m as = do
  let (xs, ys) = splitInHalf as
  xs' <- new
  ys' <- new
  fork $ put xs' (f xs)
  fork $ put ys' (f ys)
  xs'' <- get xs'
  ys'' <- get ys'
  return $ m xs'' ys''

-- | Helper function encapsulating the DaC pattern with the Par monad
dacPar' :: (NFData b) => ([a] -> Par [b]) -> TJoin b -> [a] -> [a] -> Par [b]
dacPar' f m xs ys = do
  xs'  <- new
  ys'  <- new
  fork $ join $ fmap (put xs') (f xs)
  fork $ join $ fmap (put ys') (f ys)
  xs'' <- get xs'
  ys'' <- get ys'
  return $ m xs'' ys'' 


-- | Helper function encapsulating the DaC pattern using par and pseq
dacPseq :: (NFData b) => ([a] -> [b]) -> TJoin b -> [a] -> [b]
dacPseq f m as = par (rnf ys') (pseq xs' (m xs' ys'))
  where (xs, ys) = splitInHalf as
        xs'      = f xs
        ys'      = f ys


-- * Benchmark Related

-- | Placeholder for current map strategy
jackknife :: (NFData b) => ([a] -> b) -> [a] -> [b]
-- jackknife f = map f . resamples 500
jackknife f = Main.parMap' 2 f . resamples 500
-- jackknife f = parMapRD' 2 f . resamples 500
-- jackknife f = parMapS f . resamples 500
-- jackknife f = parMapP f . resamples 500
-- jackknife f = parMapPD 3 f . resamples 500

-- | Placeholder for current sort strategy
sortfun :: (NFData a, Ord a) => [a] -> [a]
-- sortfun = mergesort
-- sortfun = mergesortPseq
sortfun = mergesortRD' 3
-- sortfun = mergesortP
-- sortfun = mergesortPD' 3

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
-- main = jackBench


