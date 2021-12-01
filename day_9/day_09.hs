import Data.Maybe

isValid :: [Int] -> Int -> Bool
isValid preamble next = length [a | a <- preamble, b <- preamble, a + b == next, a /= b] > 0

verifyXmas :: [Int] -> Maybe Int
verifyXmas [] = Nothing
verifyXmas numbers
  | length numbers < 25 = Nothing
  | isValid preamble next = verifyXmas $ tail numbers
  | otherwise = Just next
  where (preamble, (next:_)) = splitAt 25 numbers

findSet :: Int -> [Int] -> [Int]
findSet invalid input = helper input []
  where helper stream memory
          | sum mem == invalid = memory
          | sum mem < invalid = helper (tail stream) mem
          | otherwise = helper stream (tail memory)
          where mem = memory ++ [(head stream)]

findWeakness :: Int -> [Int] -> Int
findWeakness invalid numbers = minimum set + maximum set
  where set = findSet invalid numbers

main :: IO ()
main = do
  numbers <- map (read :: String -> Int) . lines <$> readFile "day_09_input.txt"

  let part1 = fromJust $ verifyXmas numbers
  print $ show part1
  print $ show (findWeakness part1 numbers)