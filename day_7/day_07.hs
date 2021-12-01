import Data.List
import Data.List.Split
import Data.Maybe

type Rule = (String, [(Int, String)])

decodeRuleComposition :: [String] -> (Int, String)
decodeRuleComposition input = (num, colour)
  where num = read $ head input
        colour = unwords $ take 2 (tail input)

decodeRule :: String -> Rule
decodeRule input
  | "no other bags" `isInfixOf` input = (key, [])
  | otherwise = (key, children)
  where parts = words input
        key = unwords $ take 2 parts
        children = [decodeRuleComposition x | x <- (chunksOf 4 (drop 4 parts))]

validContainers :: [Rule] -> String -> Int
validContainers rules bag = length $ ((helper [bag] rules) \\ [bag])
  where helper [] _ = []
        helper acc rules = nub $ containers ++ parents ++ acc
          where listContainers bag = map fst $ filter (elem bag . map snd . snd) rules
                containers = concat $ map listContainers acc
                parents = helper (containers \\ acc) rules

countBags :: [Rule] -> String -> Int
countBags rules bag = helper rules (1, bag) - 1
  where helper rules (number, colour) = number * (1 + sub)
          where contents = fromJust $ lookup colour rules
                sub = sum $ map (helper rules) contents

main :: IO ()
main = do
  rules <- map decodeRule . lines <$> readFile "day_07_input.txt"

  print $ "Part 1: " ++ (show $ validContainers rules "shiny gold")
  print $ "Part 2: " ++ (show $ countBags rules "shiny gold")
