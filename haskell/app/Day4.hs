module Main where

import Advent(split)

main :: IO ()
main = do
  input <- readFile "D:/Niklas/repos/aoc2022/data/input4.txt"
  let ranges = map parseLine (lines input)
  let fullyContained = length $ filter eitherContains ranges
  print fullyContained
  let overlapping = length $ filter (not . disjoint) ranges
  print overlapping

type Range = (Int, Int)

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
