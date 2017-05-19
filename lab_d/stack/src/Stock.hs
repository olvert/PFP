module Stock where
  
import Data.List

-- Buy date, sell date and profit
type Result = (Int, Int, Int)

-- Day has a index and a price
type Day = (Int, Int)

-- | Main solve function for stock market problem
buySell :: [Int] -> Result
buySell days = local defaultRes $ indices days
  where 
    local l [] = l
    local l (d:days) = let l' = sell d days in local (maxResult l l') days

-- | Given a buy day returns the optimal sell day and profit
sell :: Day -> [Day] -> Result
sell (i, b) days = let (j, s) = foldl' maxDay (0,0) days in (i, j, s - b)
      
-- | Default result value
defaultRes :: Result
defaultRes = (0, 0, 0)

-- | Adds indices to stock values      
indices :: [a] -> [(Int, a)]
indices = zip [0..]

-- | Returns 'max' of two days
maxDay :: Day -> Day -> Day
maxDay d1 d2 = if snd d1 >= snd d2 then d1 else d2

-- | Returns 'max' of two results
maxResult :: Result -> Result -> Result
maxResult r1 r2 = if profit r1 > profit r2 then r1 else r2

-- | Returns proft given result
profit :: Result -> Int
profit (b, s, p) = p
