import Data.Char
import Data.List
import Data.List.Split

convertToBinary :: Char -> Char
convertToBinary 'F' = '0'
convertToBinary 'B' = '1'
convertToBinary 'L' = '0'
convertToBinary 'R' = '1'
convertToBinary _ = error "Unknown character"

fromBinary :: [Char] -> Int
fromBinary "" = 0
fromBinary input = (2 * (fromBinary (init input))) + digitToInt (last input)

decodeSeat :: String -> Int
decodeSeat code = row * 8 + col
  where bin = map convertToBinary code
        row = fromBinary $ take 7 bin
        col = fromBinary $ drop 7 bin

findMissing :: [Int] -> Int
findMissing seats = head missing + 1
  where missing = [x | x:y:_ <- tails (sort seats), y-x > 1]

main :: IO ()
main = do
  seats <- map decodeSeat . lines <$> readFile "day_05_input.txt"

  print $ "Part 1: " ++ (show $ maximum seats)
  print $ "Part 2: " ++ (show $ findMissing seats)
