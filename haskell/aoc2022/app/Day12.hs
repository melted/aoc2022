import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.Maybe

main = do
    input <- inputData
    let (start, end, _, points) = parseInput input
    print $ find start (S.member end)  (\x z -> x-z<= 1) points
    print $ find end (any (\p -> points M.! p == 0)) (\x z -> z-x <= 1) points

inputData = readFile "D:/Niklas/repos/aoc2022/data/input12.txt"

parseInput input = foldl parse (origin, origin, origin, M.empty) input  
    where
        origin = (0,0)
        parse (start, end, (x, y), points) '\n' = (start, end, (0, y+1), points)
        parse (start, end, (x, y), points) 'S' = ((x,y), end, (x+1, y), M.insert (x, y) 0 points)
        parse (start, end, (x, y), points) 'E' = (start, (x,y), (x+1, y), M.insert (x, y) 25 points)
        parse (start, end, (x, y), points) ch = (start, end, (x+1, y), M.insert (x, y) (ord ch - 97) points)

neighbors (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x,y-1)]

find start cond rule points = go S.empty (S.singleton start) 0
    where
        go visited checking n | S.null checking = Nothing
        go visited checking n = if cond checking 
                                    then Just n 
                                    else go (S.union visited checking) (toCheck visited checking) (n+1)
        valid p q | Just x <- points M.!? q, Just z <- points M.!? p, rule x z = Just q
        valid _ _ = Nothing
        validNeighbors p = S.fromList $ mapMaybe (valid p) $ neighbors p
        toCheck visited checking = S.difference (S.unions $ S.map validNeighbors checking) visited
