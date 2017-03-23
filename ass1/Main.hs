module Main where

import Data.List hiding (partition)
import System.Random
import Criterion.Main
import Control.Parallel
import Utils

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

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

-- | Parallel map function using par and pseq
mapPseq :: ([a] -> b) -> [[a]] -> [b]
mapPseq f []     = []
mapPseq f (a:as) = par b (pseq bs (b:bs))
  where b  = f a
        bs = mapPseq f as

-- | Parallel map function using par and pseq
mapPseqP :: Int -> ([a] -> b) -> [[a]] -> [b]
mapPseqP s f as = mapPseqP' $ partition s as
  where mapPseqP' []      = []
        mapPseqP' (b:bs)  = par b' (pseq bs' (b'++bs'))
          where b'  = map f b
                bs' = mapPseqP' bs


jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = mapPseqP 2 f . resamples 500



crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

main = do
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
