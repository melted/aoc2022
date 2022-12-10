import Data.List
import Data.Maybe
import Data.Traversable
import qualified Data.Map as M

inputData = readFile "D:/Niklas/repos/aoc2022/data/input10.txt"

main = do
    input <- inputData
    let prog = parse input
    let output = M.fromList $ execProg prog
    let result = sum $ map (\c -> score c $ fromJust $ M.lookupLT c output) [20, 60 .. 220]
    print result
    forM (display output) $ putStrLn

data Instruction = Noop | Addx Int deriving(Eq, Ord, Show)

parse :: String -> [Instruction]
parse input = map parseInstruction (lines input) 

parseInstruction "noop" = Noop
parseInstruction x | ["addx", arg] <- words x = Addx $ read arg
parseInstruction err = error $ "invalid instruction: " ++ err

exec (cc, regX) Noop = (cc+1, regX)
exec (cc, regX) (Addx n) = (cc+2, regX+n)

execProg = scanl exec (0,1)

score n (_, reg) = n*reg

chunk n xs = splitter [] xs
    where 
        splitter acc [] = reverse acc
        splitter acc xs = let (a, b) = splitAt n xs in splitter (a:acc) b

display m = chunk 40 $ map displayCycle [0..239]
    where
        reg n = fromJust $ snd <$> M.lookupLE n m
        displayCycle n = if abs (reg n - mod n 40) <= 1 then '#' else '.'