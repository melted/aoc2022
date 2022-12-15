{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack)
import Data.Attoparsec.Text
import Data.Maybe
import Data.List
import Data.Ord
import Debug.Trace
import qualified Data.Set as S

main = do
    input <- inputData
    let points = parseAll input
    print $ sortBy (comparing fst) $ mapMaybe (project 2000000) points
    let (x,y) = findPoint points ((0,0), 4000000)
    print $ x*4000000+y

inputData = pack <$> readFile "D:/Niklas/repos/aoc2022/data/input15.txt"

type Point = (Int, Int)

parseLine :: Parser (Point, Point)
parseLine = do
    string "Sensor at x="
    x <- signed decimal
    string ", y="
    y <- signed decimal
    string ": closest beacon is at x="
    bx <- signed decimal
    string ", y="
    by <- signed decimal
    return ((x,y), (bx, by))

parseAll input = case parseOnly (sepBy parseLine endOfLine) input of
                    Left err -> error err
                    Right v -> v

range ((x, y), (x2, y2)) = abs (x-x2) + abs(y-y2)

project yy s@((x, y), (x2,y2)) = if xr >= 0 then Just (x-xr, x+xr) else Nothing
    where
        r = range s
        xr = r - abs (y-yy)

insideRange p s = range s >= range (fst s, p)

completelyCovered ((x,y), delta) s = insideRange (x,y) s && insideRange (x+delta, y) s
                                         && insideRange (x, y+delta) s && insideRange (x+delta,y+delta) s

findPoint sensors area = go [area] [] []
    where
        go [] [] [] = error "found nothing"
        go [] [] (x:xs) | any (insideRange x) sensors = go [] [] xs
        go [] [] (x:xs) = x
        go [] acc p = go acc [] p
        go ((x, 0):xs) acc p = go xs acc (x:p)
        go (x:xs) acc p | any (completelyCovered x) sensors = go xs acc p
        go (x:xs) acc p = go xs ((subdivide x)++acc) p
        subdivide ((x,y), delta) = let n = 2
                                       stride = delta `div` n
                                       steps = delta `div` stride
                                       rest = delta `mod` n
                                    in if stride > 0 
                                        then [((x+w,y+z), stride)| w <- [0, stride..stride*(steps-1)], z<-[0, stride..stride*(steps-1)]]
                                        else [((x+w, y+z), 0)| w<-[0..rest], z<-[0..rest]]
