module Utils where

-- | Splits a list in half and returns the left and right half as a tuple
splitInHalf :: [a] -> ([a], [a])
splitInHalf as = splitAt (mdl as) as

-- | Returns the 'middle index' of a list
mdl :: [a] -> Int
mdl as = length as `div` 2

-- | Takes and integer and divides it by 2
hf :: Int -> Int
hf = flip div 2
