{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack)
import Data.Attoparsec.Text
import qualified Data.Set as S


inputData = pack <$> readFile "D:/Niklas/repos/aoc2022/data/input14.txt"

main = do
    input <- inputData
    let parsed = parseAll input
    let start = S.unions (map (rocks S.empty) parsed)
    let bottom = maximum $ S.map snd start
    let result = simulate (\y _ -> y > bottom) S.member start 
    print result
    let result2 = simulate (\_ o -> S.member (500, 0) o) (\(x,y) o -> y == (bottom+2) || S.member (x,y) o) start
    print result2

rocks acc [] = acc
rocks acc [x] = acc
rocks acc ((x,y):(x2,y2):xs) | x == x2 = rocks (S.union acc (S.fromList [(x, z)| z <- [min y y2..max y y2]])) ((x2,y2):xs) 
rocks acc ((x,y):(x2,y2):xs) | y == y2 = rocks (S.union acc (S.fromList [(z, y)| z <- [min x x2..max x x2]])) ((x2,y2):xs)  
rocks _ _ = error "Angled line"

simulate endCond blockRule start = fall (500,0) start
    where
        fall (x, y) obstacles | endCond y obstacles = length obstacles - length start
        fall (x, y) obstacles | blockRule (x, y+1) obstacles = if not $ blockRule (x-1, y+1) obstacles 
                                                                  then fall (x-1, y+1) obstacles
                                                                    else if not $ blockRule (x+1, y+1) obstacles
                                                                        then fall (x+1, y+1) obstacles
                                                                        else fall (500, 0) (S.insert (x,y) obstacles)
        fall (x, y) obstacles = fall (x, y+1) obstacles

parseLine :: Parser [(Int, Int)]
parseLine = sepBy parseTuple (string " -> ")

parseTuple = do
    x <- decimal
    char ','
    y <- decimal
    return (x,y)

parseAll input = case parseOnly (sepBy parseLine endOfLine) input of
                    Left err -> error err
                    Right v -> v