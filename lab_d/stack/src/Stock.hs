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
    local l (d:days) = 
      let l' = sell d days in
      if profit l' >= profit l then local l' days
      else local l days

-- | Given a buy day returns the optimal sell day and profit
sell :: Day -> [Day] -> Result
sell (i, b) days = let (j, s) = foldl' local (0,0) days in (i, j, s - b)
  where local a b = if snd a >= snd b then a else b
      
-- | Default result value
defaultRes :: Result
defaultRes = (0, 0, 0)

-- | Returns proft given result
profit :: Result -> Int
profit (b, s, p) = p

-- | Adds indices to stock values      
indices :: [a] -> [(Int, a)]
indices = zip [0..]
