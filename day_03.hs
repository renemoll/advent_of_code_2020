-- Note: the result step is in (x,y)
step :: Int -> Int -> Int -> Int -> [(Int,Int)]
step right down xlimit ylimit = steps
  where steps = [(right*x `mod` ylimit, down*x) | x <- [0,1..xlimit], x*down<xlimit]

encounteredTrees :: [String] -> Int -> Int -> Int
encounteredTrees input right down = trees
  where ylimit = length $ head input
        xlimit = length input
        steps = step right down xlimit ylimit
        trees = length $ filter (==True) [(input !! y !! x) == '#' | (x,y) <- steps]

main :: IO ()
main = do
  input <- lines <$> readFile "day_03_input.txt"
  
  let part1 = encounteredTrees input 3 1
  print $ "Part 1: " ++ (show part1)

  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  let part2 = foldl (*) 1 [encounteredTrees input r d | (r,d) <- slopes]
  print $ "Part 2: " ++ (show part2)