import qualified Data.Map as M
import Data.Char
import Debug.Trace

inputData = readFile "D:/Niklas/repos/aoc2022/data/input22.txt"

data Instruction = L | R | Go Int deriving (Eq, Show)

facings = [(1,0), (0,1), (-1,0), (0,-1)]

turn facing L = (facing-1) `mod` (length facings)
turn facing R = (facing+1) `mod` (length facings)
turn facing _ = facing

next facing (x,y) = let (dx, dy) = facings !! facing in (x+dx, y+dy)

parse input = (parseMap $ unlines a, parseInstructions (unlines b))
    where
        (a, b) = break (== "") $ lines input
        parseInstructions [] = []
        parseInstructions ('L':xs) = L:parseInstructions xs
        parseInstructions ('R':xs) = R:parseInstructions xs
        parseInstructions xs | isDigit $ head xs = let (x,rest) = span isDigit xs in (Go $ read x):parseInstructions rest
        parseInstructions (x:xs) = parseInstructions xs

parseMap input = foldl parsePoint (origin, M.empty) input  
    where
        origin = (0,0)
        parsePoint ((x, y), points) '\n' = ((0, y+1), points)
        parsePoint ((x, y), points) ' ' = ((x+1, y), points)
        parsePoint ((x, y), points) ch = ((x+1, y), M.insert (x, y) (ch == '#') points)


move p2 (maxX, maxY, m) ((x,y), facing) (Go 0) = ((x,y), facing)
move p2 (maxX, maxY, m) ((x,y), facing) (Go n) = case M.lookup pos m of
                                                        Just False ->  move p2 (maxX, maxY, m) (pos, facing) (Go $ n-1)
                                                        Just True -> ((x,y), facing)
                                                        Nothing ->
                                                            case teleport (x,y) of
                                                                Just (p, f) -> move p2 (maxX, maxY, m) (p, f) (Go $ n-1)
                                                                Nothing -> ((x,y), facing)
        where
            pos = next facing (x,y)
            teleport = if p2 then teleport2 else teleport1
            teleport1 (x,y) = if m M.! (nx, ny) then Nothing else Just ((nx, ny), facing)
                where 
                    (nx, ny) = case facing of 
                                    0 -> (minimum $ row m y, y)
                                    1 -> (x, minimum $ col m x)
                                    2 -> (maximum $ row m y, y)
                                    3 -> (x, maximum $ col m x)
            teleport2 (x,y) =  if m M.! (nx, ny) then Nothing else Just ((nx, ny), f)
                where 
                    (nx, ny, f) = case facing of 
                                    0 | y < 50 -> (x-50, 149-y, 2)
                                    0 | y < 100 -> (y+50, 49, 3)
                                    0 | y < 150 -> (x+50, 149-y, 2)
                                    0  -> (y-100, 149, 3)
                                    1 | x < 50 -> (x+100, 0, 1)
                                    1 | x < 100 -> (49, x+100, 2)
                                    1 -> (99, x-50, 2)
                                    2 | y < 50 -> (x-50, 149-y, 0)
                                    2 | y < 100 -> (y-50, 100, 1)
                                    2 | y < 150 -> (x+50, 149-y, 0)
                                    2  -> (y-100, 0, 1)
                                    3 | x < 50 -> (50, x+50, 0)
                                    3 | x < 100 -> (0, x+100, 0)
                                    3 -> (x-100, 199, 3)
move p2 (maxX, maxY, m) ((x,y), facing) i = ((x,y), turn facing i)

col m x = map snd $ filter ((== x) . fst) $ M.keys m
row m y = map fst $ filter ((== y) . snd) $ M.keys m
main = do
    input <- inputData
    let ((_, m), i) = parse input
    let maxX = maximum $ map fst $ M.keys m
    let maxY = maximum $ map snd $ M.keys m
    let startX = minimum $ map fst $ filter ((== 0) . snd) $ M.keys m
    let ((x,y), f) = foldl (move False (maxX, maxY, m)) ((startX,0), 0) i
    print $ (y+1)*1000+(x+1)*4+f
    let ((x,y), f) = foldl (move True (maxX, maxY, m)) ((startX,0), 0) i
    print $ (y+1)*1000+(x+1)*4+f
