import Data.List.Split

uniqueAnswers :: String -> Int
uniqueAnswers = helper []
  where helper seen [] = length seen
        helper seen (x:xs)
          | x `elem` seen = helper seen xs
          | otherwise = helper (seen ++ [x]) xs

countAnswers :: [String] -> Int
countAnswers input = uniqueAnswers $ concat input

countAgreedAnswers :: [String] -> Int
countAgreedAnswers (x:xs) = helper x xs
  where helper seen [] = uniqueAnswers seen
        helper seen (x:xs) = helper [c  | c <- x, c `elem` seen] xs

main :: IO ()
main = do
  groups <- splitOn "\n\n" <$> readFile "day_06_input.txt"

  let part1 = sum $ map (countAnswers . lines) groups
  print $ "Part 1: " ++ (show part1)

  let part2 = sum $ map (countAgreedAnswers . lines) groups
  print $ "Part 2: " ++ (show part2)
