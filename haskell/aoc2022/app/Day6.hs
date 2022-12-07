import Data.List

main = do
    input <- inputData
    print (findPacket 4 input)
    print (findPacket 14 input)

inputData = readFile "D:/Niklas/repos/aoc2022/data/input6.txt"

allUnique xs = length (nub xs) == length xs

windows n xs = [ take n m | m <- tails xs ]

findPacket n xs = (n+) <$> findIndex allUnique (windows n xs)