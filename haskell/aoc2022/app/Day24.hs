import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as I
import Data.Maybe
import Debug.Trace

inputData = readFile "../../data/input24.txt"

data Wind = N | S | W | E deriving (Show, Eq, Ord, Enum)

data State = State {
    north :: S.Set (Int, Int),
    south :: S.Set (Int, Int),
    west :: S.Set (Int, Int),
    east :: S.Set (Int, Int),
    walls :: S.Set (Int, Int)
} deriving (Eq, Show)

emptyState = State S.empty S.empty S.empty S.empty S.empty

parseMap :: String -> State 
parseMap input = snd $ foldl parsePoint (origin, emptyState) input  
    where
        origin = (0,0)
        parsePoint ((x, y), state) '\n' = ((0, y+1), state)
        parsePoint ((x, y), points) '.' = ((x+1, y), points)
        parsePoint ((x, y), state) '#' =
            ((x+1, y), state { walls = S.insert (x,y) (walls state) })
        parsePoint ((x, y), state) '^' =
            ((x+1, y), state { north = S.insert (x,y) (north state) })
        parsePoint ((x, y), state) 'v' =
            ((x+1, y), state { south = S.insert (x,y) (south state) })
        parsePoint ((x, y), state) '<' =
            ((x+1, y), state { west = S.insert (x,y) (west state) })
        parsePoint ((x, y), state) '>' =
            ((x+1, y), state { east = S.insert (x,y) (east state) })

updateState state = updateNorth $ updateSouth $ updateEast $ updateWest state
    where
        maxX = (maximum $ map fst $ S.toList (walls state))-1
        maxY = (maximum $ map snd $ S.toList (walls state))-1
        minX = 1+(minimum $ map fst $ S.toList (walls state))
        minY = 1+(minimum $ map snd $ S.toList (walls state))
        updateNorth s = s { north = S.map (\(x,y)->if y>minY then (x,y-1) else (x,maxY)) $ north s }
        updateSouth s = s { south = S.map (\(x,y)->if y<maxY then (x,y+1) else (x,minY)) $ south s }
        updateEast s = s { east = S.map (\(x,y)->if x<maxX then (x+1,y) else (minX,y)) $ east s }
        updateWest s = s { west = S.map (\(x,y)->if x>minX then (x-1,y) else (maxX,y)) $ west s }

moves (x,y) = filter (\(x,y)-> y>=0) [(x,y),(x,y-1),(x,y+1),(x-1,y),(x+1,y)]

invalidMoves state = S.unions [east state, west state, north state,
                               south state, walls state]
findWay target start state t = go t (S.singleton start)
    where
        winds = I.fromList $ zip [0..10000] (iterate updateState state) 
        go t toCheck | S.member target toCheck = t
        go t toCheck = go (t+1) nextCheck 
            where
                current = winds I.! (t+1)
                next xy = S.fromList (moves xy) S.\\ (invalidMoves current)
                nextCheck = S.unions (S.map next toCheck)

main = do
    input <- inputData
    let state = parseMap input
    let maxX = maximum $ map fst $ S.toList (walls state)
    let maxY = maximum $ map snd $ S.toList (walls state)
    let res = findWay (maxX-1,maxY) (1,0) state 0
    print res
    let t = findWay (1,0) (maxX-1, maxY) state res
    let res2 = findWay (maxX-1,maxY) (1,0) state t
    print res2
