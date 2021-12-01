import Data.List
import Data.List.Split

countAnswers :: [String] -> Int
-- from a list of charachters, count the unique ones (nub removes dublicates)
countAnswers input = (length . nub . concat) input

countAgreedAnswers :: [String] -> Int
-- from a list of characters, count the character present in all entries (intersect determines the common elements)
countAgreedAnswers input = (length . foldl1 intersect) input

main :: IO ()
main = do
  -- Split the text into blocks
  groups <- map lines . splitOn "\n\n" <$> readFile "day_06_input.txt"

  let part1 = sum $ map countAnswers groups
  print $ "Part 1: " ++ (show part1)

  let part2 = sum $ map countAgreedAnswers groups
  print $ "Part 2: " ++ (show part2)
