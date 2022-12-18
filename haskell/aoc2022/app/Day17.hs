import qualified Data.Set as S
import Data.Maybe
import Data.Char
import qualified Data.Map as M

inputData = readFile "D:/Niklas/repos/aoc2022/data/input17.txt"

testData = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

main = do
    input <- inputData
    let clean = filter (not . isControl) input
    let play = iterate playPiece (S.empty, cycle shapes, cycle clean,0)
    let (obstacles,_,_,m ) = play !! 2022
    print $ height obstacles
    let (cycle, n, dh) = findRepeat clean
    let (cycles, offset) = (1000000000000 - n) `divMod` cycle
    let (obs,_,_,m) = play !! (offset+n)
    let starth = height obs
    print $ starth + dh*cycles

vrod = [(0,0),(1,0),(2,0),(3,0)]
cross = [(0,1),(1,0),(1,1),(1,2),(2,1)]
bend = [(0,0),(0,1),(0,2),(1,2),(2,2)]
hrod = [(0,0),(0,1),(0,2),(0,3)]
box = [(0,0), (0,1),(1,0),(1,1)]

shapes = [hrod,cross, bend, vrod, box]

translate (y,x) shape = map (\(w,z) ->(y+w, x+z)) shape

move (y,x) shape obstacles = if checkXBounds && checkFloor && checkObstacles
                                then Just newShape
                                else Nothing
    where
        newShape = translate (y,x) shape
        checkXBounds = (minimum $ map snd newShape) >= 0 && (maximum $ map snd newShape) <= 6
        checkFloor = (minimum $ map fst newShape) > 0
        checkObstacles = null $ filter (`S.member` obstacles) newShape 

playPiece (obstacles, (s:shapes), jets, n) = go obstacles shapes jets (startPos s) (n+2)
    where
        startPos :: [(Int, Int)] -> [(Int, Int)]
        startPos shape = translate (4+fromMaybe 0 (fst <$> S.lookupMax obstacles),2) shape
        go obstacles shapes (j:jets) piece m = case vertical of
                                                Just p -> go obstacles shapes jets p (m+2)
                                                Nothing -> (foldr (S.insert) obstacles lateral, shapes, jets, m)
            where
                lateral = fromMaybe piece $ move (0, if j == '<' then -1 else 1) piece obstacles
                vertical = move (-1,0) lateral obstacles

extractTop xs = S.filter (\(y,x) -> y >= lowest) xs
    where
        cols = map (\x -> S.filter ((== x) . snd) xs) [0..6]
        lowest = minimum $ map (fromMaybe 0 . S.lookupMax . S.map fst) cols

normalize xs = S.map (\(y,x) -> (y-mm,x)) xs
    where
        mm = fromMaybe 0 $ S.lookupMin (S.map fst xs)

height obs = fst $ fromMaybe (0,0) $ S.lookupMax obs

findRepeat jets = go M.empty (S.empty, cycle shapes, cycle jets, 0) 0
    where
        jetCycle = length jets
        shapeCycle = length shapes
        norm obs = normalize $ extractTop obs
        checkState memo (obs, _,_,jn) m = M.lookup (norm obs, m `mod` shapeCycle, jn `mod` jetCycle) memo
        go memo state@(obs,_,_,_) m | Just (n,h) <- checkState memo state m = ((m-n), n, height obs- h) 
        go memo state m = go (updatedMemo memo state m) (playPiece state) (m+1)
        updatedMemo memo (obs,_,_,jn) m = M.insert (norm obs, m `mod` shapeCycle, jn `mod` jetCycle) (m,height obs) memo