module Utils where

-- | Partitions a list into a list of chunks where the number of chunks is
-- determined by the int argument.
partition :: Int -> [a] -> [[a]]
partition n as = partition' as
  where cs = max 1 (length as `div` n) -- cannot have more chunks than elements
        partition' [] = []
        partition' as' = take cs as' : partition' (drop cs as')
