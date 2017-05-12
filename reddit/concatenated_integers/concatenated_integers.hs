less_than :: String -> String -> Bool
less_than n m = (n ++ m) < (m ++ n)

more_than :: String -> String -> Bool
more_than n m = less_than m n

quicksort :: (String -> String -> Bool) -> [String] -> [String]
quicksort _ [] = []
quicksort sort (x:xs) =
  let smaller = quicksort sort [z | z <- xs, sort z x]
      bigger = quicksort sort [z | z <- xs, sort x z]
  in smaller ++ [x] ++ bigger

exec :: [String] -> [(String, String)]
exec xs = zip (fmap normal_sort xs) (fmap reversed_sort xs)
  where
    normal_sort = concat . (quicksort less_than) . words
    reversed_sort = concat . (quicksort more_than) . words

main = do
  s <- readFile $ "concatenated_integers.txt"
  print $ exec . lines $ s
