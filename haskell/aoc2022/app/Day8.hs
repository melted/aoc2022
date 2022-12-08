import Data.List (zipWith)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

inputData = readFile "D:/Niklas/repos/aoc2022/data/input8.txt"

main = do
    input <- inputData
    let (w, h, d) = parseInput input
    let sightLines = allLines w h
    let result = S.unions (map (checkLOS d) sightLines)
    print $ length result
    let scores = fmap (checkPoint d) $ M.keys d
    print $ maximum scores

parseInput :: String -> (Int, Int, M.Map (Int, Int) Int)
parseInput input = (x, y, M.fromList $ concat $ map col rows)
    where 
        rows = zip [0..] (lines input)
        y = length rows - 1
        x = length (snd $ head rows) - 1
        col (r, row) = zipWith (\c n -> ((c, r), read [n])) [0..] row

horizontal w y = zip [0..w] (repeat y) 
vertical h x = zip (repeat x) [0..h]

horizontals h w = map (horizontal w) [0..h]
verticals h w = map (vertical h) [0..w]

allLines h w = concat [horizontals h w, 
                       verticals h w,
                       map reverse (horizontals h w),
                       map reverse (verticals h w)]

dirs = [\(x, y) -> (x+1, y),
        \(x, y) -> (x-1, y),
        \(x, y) -> (x, y+1),
        \(x, y) -> (x, y-1)]

checkPoint m p = product $ map (\f -> go f (f p) 0) dirs
    where
        h = m M.! p
        go f pos count = case M.lookup pos m of 
                            Just t -> if t < h then go f (f pos) (count + 1) else count+1
                            Nothing -> count

checkLOS m los = snd $ foldr check (-1, S.empty) vals
    where
        vals = mapMaybe (\p -> (p,) <$> M.lookup p m) los
        check (p, h) (maxh, s) = if h > maxh then (h, S.insert p s) else (maxh, s)
