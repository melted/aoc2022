
import Data.Bifunctor(second)
import qualified Data.Set as S

inputData = readFile "D:/Niklas/repos/aoc2022/data/input9.txt"

main = do
    input <- inputData
    let cmds = map parseLine $ lines input
    let (rope, visited) = foldl evolve ([(0,0), (0,0)], S.singleton (0,0)) cmds
    print $ length visited
    let (rope, visited) = foldl evolve (replicate 10 (0,0), S.singleton (0,0)) cmds
    print $ length visited

parseLine :: String -> (String, Int)
parseLine str = second read $ break (== ' ') str

move "R" = \(x,y)->(x+1,y)
move "L" = \(x,y)->(x-1,y)
move "U" = \(x,y)->(x,y+1)
move "D" = \(x,y)->(x,y-1)

delta (x1,y1) (x2, y2) = (x1-x2, y1-y2)

tailMove (x, y) | abs x < 2 && abs y < 2 = id
                | abs x > 2 || abs y > 2 = error "detached tail"
                | otherwise = \(z, w) -> (z+signum x, w+signum y)

evolve state (dir, count) = go count state  
    where
        go 0 state = state
        go n (rope, visited) = go (n-1) (reverse revRope, S.insert (head revRope) visited)
            where
                newHead = move dir (head rope)
                revRope = foldl (\nr t -> (update nr t):nr) [newHead] (tail rope)
                update rope knot = tailMove (delta (head rope) knot) knot