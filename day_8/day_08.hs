import Data.Maybe

data Opcode = ACC
            | JMP
            | NOP
            deriving (Eq, Show)

data Instruction = Instruction {
  opcode :: Opcode,
  argument :: Int
} deriving (Show)

parse :: String -> Instruction
parse input = Instruction t (sign * arg)
  where t = conv $ take 3 input
        sign = if (input !! 4 == '+') then 1 else -1
        arg = read (drop 5 input) :: Int
        conv name
          | name == "acc" = ACC
          | name == "jmp" = JMP
          | otherwise = NOP

execute :: [Instruction] -> Int
execute insts = exec 0 0 []
  where exec ip acc seen
          | ip `elem` seen = acc
          | t == ACC = exec (ip + 1) (acc + a) (ip:seen)
          | t == JMP = exec (ip + a) acc (ip:seen)
          | t == NOP = exec (ip + 1) acc (ip:seen)
          where inst = insts !! ip
                t = opcode inst
                a = argument inst

execute' :: [Instruction] -> Maybe Int
execute' insts = exec 0 0 []
  where exec ip acc seen
          | ip `elem` seen = Nothing
          | ip >= (length insts) = Just acc
          | t == ACC = exec (ip + 1) (acc + a) (ip:seen)
          | t == JMP = exec (ip + a) acc (ip:seen)
          | t == NOP = exec (ip + 1) acc (ip:seen)
          where inst = insts !! ip
                t = opcode inst
                a = argument inst

swapInstruction :: Instruction -> Instruction
swapInstruction (Instruction JMP a) = Instruction NOP a
swapInstruction (Instruction NOP a) = Instruction JMP a
swapInstruction x = x

changeProgram :: [Instruction] -> Int -> [Instruction]
changeProgram insts ip = let (ys, zs) = splitAt ip insts in ys ++ [(swapInstruction $ head zs)] ++ (tail zs )

fixApplication :: [Instruction] -> Int
fixApplication insts = head $ catMaybes $ map execute' $ map (changeProgram insts) [0.. ]

main :: IO ()
main = do
  instructions <- map parse . lines <$> readFile "day_08_input.txt"

  print $ "Part 1: " ++ (show $ execute instructions)
  print $ "Part 2: " ++ (show $ fixApplication instructions)
