-- Used this algorithm: https://stackoverflow.com/questions/15580291/how-to-efficiently-calculate-a-row-in-pascals-triangle

import Data.List

-- Basically this is an optimized factorial
calc_paths :: Integer -> Integer -> Integer
calc_paths m n = foldl' f 1 [2..n] `mod` (10^9+7)
  where f z i = z*(m+n-i) `div` (i-1)

-- use the symmetry of the problem
paths m n
  | m<n = calc_paths n m
  | otherwise = calc_paths m n

getUserInput count = do
  line <- getLine
  let l = map read $ words line :: [Integer]
  print (paths (l!!0) (l!!1))
  if count > 1
    then getUserInput $ count - 1
    else return line

main = do
  count <- getLine
  getUserInput (read count :: Integer)
