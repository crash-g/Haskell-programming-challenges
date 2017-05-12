-- The solutions to some of the "Five programming problems every Software Engineer should be able to solve in less than 1 hour" (see http://www.shiftedup.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour).

import Data.List

-- Given two lists, create a list which alternates the elements
combine_lists :: [a] -> [a] -> [a]
combine_lists [] ys = ys
combine_lists xs [] = xs
combine_lists (x:xs) (y:ys) = x : y : combine_lists xs ys

-- Efficient fibonacci generator
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = snd . foldl' (\(x,y) z -> (y, x+y)) (0,1) $ [2..n]

-- Write all ways of getting 100 by adding, subtracting or concatenating digits from 1 to 9,
-- without changing their order (brute-force solution)
data Ops = Add | Subtract | Concat deriving (Show)

apply :: Ops -> Integer -> Integer -> Integer
apply Add n m = n+m
apply Subtract n m = n-m
apply Concat n m = read $ (show n) ++ (show m) :: Integer

-- Apply a list of Ops to a list of Integers: size [Ops] must be size [Integer] - 1
apply_to_list :: [Ops] -> [Integer] -> Integer
apply_to_list [f] [x,y] = apply f x y
apply_to_list (f:Concat:fs) (x:y:z:ls) = apply_to_list (f:fs) (x : apply Concat y z : ls)
apply_to_list (f:fs) (x:y:ls) = apply_to_list fs (apply f x y : ls)

-- Create a visual representation of the action of apply_to_list
show_ops :: [Ops] -> [Integer] -> String
show_ops xf xs = show_func (Concat : xf) xs ""
  where show_func [Add] [x] s = s ++ "+" ++ show x
        show_func [Subtract] [x] s = s ++ "-" ++ show x
        show_func [Concat] [x] s = s ++ show x
        show_func (f:fs) (x:ls) s = show_func fs ls (show_func [f] [x] s)

-- Create all possible lists of 8 elements taken from [Add, Subtract, Concat]
make_list_ops :: [[Ops]]
make_list_ops = mapM (const [Add, Subtract, Concat]) [1..8]

main = do
  print $ combine_lists [1,2,3] [4,5,6,7,8,9]
  print $ fmap fib [0..100]
  let validOps = filter ((==100) . fst) (fmap (\x -> (apply_to_list x [1..9],x)) make_list_ops)
  print $ unlines (fmap (\(x,y) -> show_ops y [1..9] ++ " = 100") validOps)
