
get2020_2part_product :: FilePath -> IO Int
get2020_2part_product path = do
  -- read the file line by line and convert each line to an Int
  numbers <- map (read :: String -> Int) . lines <$> readFile path
  let result = head [a*b | a <- numbers, b <- numbers, a + b == 2020]
  return result

get2020_3part_product :: FilePath -> IO Int
get2020_3part_product path = do
  numbers <- map (read :: String -> Int) . lines <$> readFile path
  let result = head [a*b*c | a <- numbers, b <- numbers, c <- numbers, a + b + c == 2020]
  return result

main :: IO ()
main = do
  result2 <- get2020_2part_product "day_01_input.txt"
  print result2
  result3 <- get2020_3part_product "day_01_input.txt"
  print result3
