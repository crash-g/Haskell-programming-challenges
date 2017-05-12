-- Used this algorithm: https://stackoverflow.com/questions/15580291/how-to-efficiently-calculate-a-row-in-pascals-triangle

import Data.List
import Data.Ratio

-- Basically this is an optimized factorial
-- which uses the symmetry of the problem
calc_paths :: Integer -> Integer -> Rational
calc_paths m n = foldl' f (1%1) [2..mn]
  where
    f z i = (num*(m+n-i)) % (den*(mn-i+1))
      where
        num = numerator z
        den = denominator z      
    mn = min m n

paths m n = numerator (calc_paths m n) `mod` (10^9 + 7)

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
