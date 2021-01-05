import Data.List

fromBinary :: [Char] -> Int
fromBinary "" = 0
fromBinary input = sum [if c `elem` "BR" then 2^i else 0 | (c,i) <- zip (reverse input) [0,1..]]

decodeSeat :: String -> Int
decodeSeat code = row * 8 + col
  where row = fromBinary $ take 7 code
        col = fromBinary $ drop 7 code

findMissing :: [Int] -> Int
findMissing seats = head missing + 1
  where missing = [x | x:y:_ <- tails (sort seats), y-x > 1]

main :: IO ()
main = do
  seats <- map decodeSeat . lines <$> readFile "day_05_input.txt"

  print $ "Part 1: " ++ (show $ maximum seats)
  print $ "Part 2: " ++ (show $ findMissing seats)
