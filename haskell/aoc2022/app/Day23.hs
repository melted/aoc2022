import qualified Data.Map as M

inputData = readFile "D:/Niklas/repos/aoc2022/data/input23.txt"

parseMap :: String -> M.Map (Int, Int) (Maybe (Int, Int))
parseMap input = snd $ foldl parsePoint (origin, M.empty) input  
    where
        origin = (0,0)
        parsePoint ((x, y), points) '\n' = ((0, y+1), points)
        parsePoint ((x, y), points) '.' = ((x+1, y), points)
        parsePoint ((x, y), points) '#' = ((x+1, y), M.insert (x, y) Nothing points)

main = do 
    input <- inputData
    let m = parseMap input
    print m
