module Stock where
  
import Data.List

-- Buy date, sell date and profit
type Result = (Int, Int, Int)

-- Day has a index and a price
type Day = (Int, Int)

-- | Main solve function for stock market problem
buySell :: [Int] -> Result
buySell []       = defaultRes
buySell days = buySellRec defaultRes $ indices days
  where 
    buySellRec (b, s, p) [] = (b, s, p)
    buySellRec (b, s, p) (d:days) = 
      let (b', s', p') = sell' d days in
      if p' >= p then buySellRec (b', s', p') days
      else buySellRec (b, s, p) days

-- | Adds indices to stock values      
indices :: [a] -> [(Int, a)]
indices = zip [0..]

-- | Given a buy day returns the optimal sell day and profit
sell :: Day -> [Day] -> Result
sell (i, b) days = let (j, p) = sellRec (0,0) days in (i, j, p)
  where
    sellRec (j, p) [] = (j, p)
    sellRec (j, p) ((k, s):days) =
      if s - b > p then sellRec (k, s-b) days
      else sellRec (j, p) days
      
sell' :: Day -> [Day] -> Result
sell' (i, b) = foldl' localMax defaultRes
  where localMax (i', j', p') (j, s) = 
          if s - b > p' then (i, j, s - b)
          else (i', j', p')
      
-- | Default result value
defaultRes :: Result
defaultRes = (0, 0, 0)

