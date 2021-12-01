
get2020_2part_product :: [Int] -> Int
get2020_2part_product numbers = head [a*b | a <- numbers, b <- numbers, a + b == 2020]

get2020_3part_product :: [Int] -> Int
get2020_3part_product numbers = head [a*b*c | a <- numbers, b <- numbers, c <- numbers, a + b + c == 2020]

main :: IO ()
main = do
  -- read the file line by line and convert each line to an Int
  numbers <- map (read :: String -> Int) . lines <$> readFile "day_01_input.txt"

  print $ get2020_2part_product numbers
  print $ get2020_3part_product numbers
