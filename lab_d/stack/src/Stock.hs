module Stock where
  
import Data.List

-- Buy date, sell date and profit
type Result = (Int, Int, Int)

-- Day has a index and a price
type Day = (Int, Int)

-- | 1: Brute force solution with O(n^2)
buySell :: [Int] -> Result
buySell days = local defRes $ indices days
  where 
    local l [] = l
    local l (d:days) = let l' = sell d days in local (maxResult l l') days
    
-- | 2: Divide and conquer solution with O(n log n)
buySellDaQ :: [Int] -> Result
buySellDaQ days = daq (length days) $ indices days
  where
    daq l []  = defRes
    daq l [d] = defRes
    daq l days = maxResult' x (maxResult' y z)
      where
        (first, second) = splitInHalf l days
        l' = (l `div` 2) + 1
        (i, b) = foldl' minDay defDay first
        (j, s) = foldl' maxDay defDay second
        x = daq l' first
        y = (i, j, s - b)
        z = daq l' second

-- | 3: Dynamic programming solution with O(n)        
buySellDyn :: [Int] -> Result
buySellDyn ns = let (d1, r) = foldl' dyna (defDay, defRes) $ indices ns in r

dyna :: (Day, Result) -> Day -> (Day, Result)
dyna (d1, r) d2 = (d1', r')
  where d1' = minDay d1 d2
        r'  = maxResult' r (fst d1, fst d2, snd d2 - snd d1)

-- | Given list, splits list in half and returns halves as tuple      
splitInHalf :: Int -> [a] -> ([a], [a])
splitInHalf n = splitAt (n `div` 2)

-- | Given a buy day returns the optimal sell day and profit
sell :: Day -> [Day] -> Result
sell (i, b) days = let (j, s) = foldl' maxDay defDay days in (i, j, s - b)
      
-- | Default result value
defRes :: Result
defRes = (0, 0, 0)

-- | Default day value
defDay :: Day
defDay = (0, 0)

-- | Adds indices to stock values      
indices :: [a] -> [(Int, a)]
indices = zip [0..]

-- | Returns 'max' of two days
maxDay :: Day -> Day -> Day
maxDay d1 d2 = if snd d1 >= snd d2 then d1 else d2

-- | Returns 'min' of two days
minDay :: Day -> Day -> Day
minDay d1 d2 = if snd d1 < snd d2 then d1 else d2

-- | Returns 'max' of two results
maxResult :: Result -> Result -> Result
maxResult r1 r2 = if profit r1 > profit r2 then r1 else r2

-- | Returns 'max' of two results
maxResult' :: Result -> Result -> Result
maxResult' r1 r2 | profit r1 > profit r2 = r1
                 | profit r1 < profit r2 = r2
                 | otherwise = maxResultOnBuy
   where 
     -- Assumes profit is equal
     maxResultOnBuy :: Result
     maxResultOnBuy | fst' r1 > fst' r2 = r1
                    | fst' r1 < fst' r2 = r2
                    | otherwise = maxResultOnSell
    
     -- Assumes profit and buy date is equal
     maxResultOnSell :: Result
     maxResultOnSell | snd' r1 < snd' r2 = r1
                     | otherwise = r2


-- | Returns buy day given result
fst' :: Result -> Int
fst' (b, s, p) = b

-- | Returns sell day given result
snd' :: Result -> Int
snd' (b, s, p) = s

-- | Returns proft given result
profit :: Result -> Int
profit (b, s, p) = p
