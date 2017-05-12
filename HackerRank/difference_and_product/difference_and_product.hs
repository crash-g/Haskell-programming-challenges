import Data.List

-- efficient Integer square root
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
  in  head $ dropWhile (not . isRoot) iters

-- get solution, up to sign symmetry
get_sol_seed :: Integer -> Integer -> [(Integer, Integer)]
get_sol_seed d p
  | d<0 = []
  | sqr*sqr == disc = let b1 = (d+sqr) `div` 2
                          b2 = (d-sqr) `div` 2
                      in if b1 == b2
                         then [(b1+d,b1)]
                         else [(b1+d,b1),(b2+d,b2)]
  | otherwise = []
    where disc = d*d+4*p
          sqr = if disc>=0 then squareRoot disc else 0

-- take sign symmetry into account
shuffle_and_clean_seed :: [(Integer, Integer)] -> [(Integer, Integer)]
shuffle_and_clean_seed [] = []
shuffle_and_clean_seed ((x,y):xs) = nub([(x,y),(-x,-y)] ++ shuffle_and_clean_seed xs)

getUserInput count = do
  line <- getLine
  let l = map read $ words line :: [Integer]
  print $ (length . shuffle_and_clean_seed) (get_sol_seed (l!!0) (l!!1))
  if count > 1
    then getUserInput $ count - 1
    else return line

main = do
  count <- getLine
  getUserInput (read count :: Integer)
