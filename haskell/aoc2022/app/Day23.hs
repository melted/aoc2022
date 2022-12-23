import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace

inputData = readFile "../../data/input23.txt"

parseMap :: String -> S.Set (Int, Int)
parseMap input = snd $ foldl parsePoint (origin, S.empty) input  
    where
        origin = (0,0)
        parsePoint ((x, y), points) '\n' = ((0, y+1), points)
        parsePoint ((x, y), points) '.' = ((x+1, y), points)
        parsePoint ((x, y), points) '#' = ((x+1, y), S.insert (x, y) points)

checkNorth (x,y) = ([(x-1,y-1),(x,y-1),(x+1,y-1)],(x,y-1))
checkSouth (x,y) = ([(x-1,y+1),(x,y+1),(x+1,y+1)],(x,y+1))
checkWest (x,y) = ([(x-1,y-1), (x-1,y),(x-1,y+1)],(x-1,y))
checkEast (x,y) = ([(x+1,y-1), (x+1,y), (x+1,y+1)],(x+1,y))
neighbours (x,y) = [(x+dx,y+dy)| dx<-[-1,0,1], dy<-[-1,0,1], (dx,dy)/=(0,0)]
checks = [checkNorth, checkSouth, checkWest, checkEast]

play (elves, checks, _) = (update elves, tail checks ++ [head checks], length valid) 
    where
        check [] (x,y) = Nothing
        check (c:cs) (x,y) = 
            let (cells, go) = c (x,y) in
                if any (`S.member` elves) cells
                    then check cs (x,y)
                    else Just go
        hasNeighbor xy = any  (`S.member` elves) $ neighbours xy
        plan xy = if hasNeighbor xy
                    then (xy, check checks xy)
                    else (xy, Nothing)
        plans = S.map plan elves
        addPlan m (xy, Just pos) = M.insertWith (++) pos [xy] m
        addPlan m _ = m
        planMap =  foldl addPlan M.empty plans
        valid =  M.map head $ M.filter ((== 1) . length) planMap
        update ev = foldl (\s (n,o) -> S.insert n $ S.delete o s) ev $ M.assocs valid

score elves = (maxX - minX+1)*(maxY- minY+1) -length elves
    where 
        maxX = maximum $ map fst $ S.toList elves
        maxY = maximum $ map snd $ S.toList elves
        minX = minimum $ map fst $ S.toList elves
        minY = minimum $ map snd $ S.toList elves

main = do 
    input <- inputData
    let m = parseMap input
    let rounds = iterate play (m, checks, 1)
    let (res,ch,v) = rounds !! 10
    print $ score res
    let l = takeWhile (\(_,_,v) -> v > 0) rounds
    print $ length l
