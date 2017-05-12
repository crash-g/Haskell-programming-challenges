-- How to read lines from a file and put them in a list of lists
-- Data.Text is used: main difference is that reading is a non-lazy operation

import qualified Data.Text as T
import qualified Data.Text.IO as T

iszerosum xs = foldr isinset False xs
  where isinset _ True = True
        isinset x False = elem x [-y | y <- xs, y <= 0]

multiiszerosum xs = fmap iszerosum xs

toint :: T.Text -> Integer
toint c = read . T.unpack $ c :: Integer

main = do
  ls <- fmap T.lines (T.readFile "lists_of_integers.txt")
  let multils = fmap ((fmap toint) . (T.split (==','))) ls
  print (multiiszerosum multils)
