import Data.List.Split

data Password = Password {
  password :: String,
  check :: Char,
  low :: Int,
  high :: Int
}

parse :: String -> Password
parse input = Password password check low high
  where [crit, password] = splitOn ": " input
        [limits, checks] = splitOn " " crit
        [lows, highs] = splitOn "-" limits
        check = head checks
        low = read lows
        high = read highs

validateAmount :: Password -> Bool
validateAmount (Password pw c low high) = result
  where t = length $ filter (==c) pw
        result = (t >= low) && (t <= high)

validatePosition :: Password -> Bool
validatePosition (Password pw c low high) = result
  where result = ((pw !! (low - 1)) == c) /= ((pw !! (high - 1)) == c)

main :: IO ()
main = do
  entries <- map parse . lines <$> readFile "day_02_input.txt"
  let result = length (filter validateAmount entries)
  print $ "Part 1: " ++ (show result)

  let result2 = length (filter validatePosition entries)
  print $ "Part 2: " ++ (show result2)
