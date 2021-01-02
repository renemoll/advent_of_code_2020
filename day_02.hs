import Data.List.Split

parse :: [Char] -> ([Char], Char, Int, Int)
parse input = (password, check, low, high)
  where [crit, password] = splitOn ": " input
        [limits, checks] = splitOn " " crit
        [lows, highs] = splitOn "-" limits
        check = head checks
        low = read lows
        high = read highs

validateAmount :: ([Char], Char, Int, Int) -> Bool
validateAmount (pw, c, low, high) = result
  where t = length $ filter (==c) pw
        result = (t >= low) && (t <= high)

validatePosition :: ([Char], Char, Int, Int) -> Bool
validatePosition (pw, c, low, high) = result
  where result = ((pw !! (low - 1)) == c) /= ((pw !! (high - 1)) == c)

main :: IO ()
main = do
  entries <- map parse . lines <$> readFile "day_02_input.txt"
  let result = length (filter validateAmount entries)
  print $ "Part 1: " ++ (show result)

  let result2 = length (filter validatePosition entries)
  print $ "Part 2: " ++ (show result2)
