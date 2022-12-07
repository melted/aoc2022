module Main where

import Advent(split)

main :: IO ()
main = do
  input <- readFile "D:/Niklas/repos/aoc2022/data/input4.txt"
  let ranges = map parseLine (lines input)
  print $ fullyContained ranges
  print $ overlapping ranges

type Range = (Int, Int)

fullyContained xs = length $ filter eitherContains xs
overlapping xs = length $ filter (not . disjoint) xs

eitherContains :: (Range, Range) -> Bool
eitherContains (a, b) = contains a b || contains b a

contains :: Range -> Range -> Bool
contains (a,b) (c,d) = a <= c && b >= d

disjoint :: (Range, Range) -> Bool
disjoint ((a,b), (c,d)) = b < c || a > d

parseLine :: String -> (Range, Range)
parseLine s = (a, b)
    where
      a:b:_ = map parseRange (split ',' s)
      parseRange t = let x:y:_ = split '-' t in (read x, read y)

testData = map parseLine $ lines "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"