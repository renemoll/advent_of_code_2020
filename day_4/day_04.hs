import Data.List.Split
import Data.Char
import Text.Read

parse :: String -> (String, String)
parse input = let [key, value] = splitOn ":" input in (key, value)

keys :: [(a, b)] -> [a]
keys passport = map fst passport

countValidFields :: [[(String, String)]] -> Int
countValidFields input = result
  where requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        result = length $ filter (==True) [and [field `elem` (keys passport) |  field <- requiredFields]  | passport <- input]

verifyLimit :: Int -> Int -> String -> Bool
verifyLimit min max test = result
  where t = read test :: Int
        result = (t >= min) && (t <= max)

verifyHeight :: String -> Bool
verifyHeight test
  | u == "cm" = (h >= 150) && (h <= 193)
  | u == "in" = (h >= 59) && (h <= 76)
  | otherwise = False
  where h = read (filter isDigit test) :: Int
        u = filter isAlpha test

verifyHairColour :: String -> Bool
verifyHairColour test = hash && code
  where hash = '#' == head test
        code = (6 == length (filter isAlphaNum test))

verifyEyeColour :: String -> Bool
verifyEyeColour test = test `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

verifyPassport :: String -> Bool
verifyPassport test = 9 == length test && all isDigit test

verifyField :: String -> Maybe String -> Bool
verifyField _ Nothing = False
verifyField name (Just input) = case name of
  "byr" -> verifyLimit 1920 2002 input
  "iyr" -> verifyLimit 2010 2020 input
  "eyr" -> verifyLimit 2020 2030 input
  "hgt" -> verifyHeight input
  "hcl" -> verifyHairColour input
  "ecl" -> verifyEyeColour input
  "pid" -> verifyPassport input
  "cid" -> True
  _ -> False

countValidatedFields :: [[(String, String)]] -> Int
countValidatedFields input = result
  where requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        result = length $ filter (==True) [and [field `elem` (keys passport) && verifyField field (lookup field passport) |  field <- requiredFields]  | passport <- input]

main :: IO ()
main = do
  -- Split the file on a double newline to split the text on passport data.
  -- For each text block with data, split on the spaces to extract the key value pairs
  -- Finally parse the pairs into a list of key value tupples.
  input <- map (map parse . words) . splitOn "\n\n" <$> readFile "day_04_input.txt"

  print $ "Part 1: " ++ (show $ countValidFields input)
  print $ "Part 2: " ++ (show $ countValidatedFields input)
