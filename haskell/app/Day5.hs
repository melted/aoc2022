module Main where

import Data.Char
import Data.List
import qualified Data.IntMap as IM

type Move = (Int, Int, Int)
type State = IM.IntMap String

main :: IO ()
main = do
  input <- inputData
  let (start, moves) = parse input
  let finish = foldl' performMove start moves
  print (map head (IM.elems finish))
  let finish2 = foldl' performMove2 start moves
  print (map head (IM.elems finish2))

inputData = readFile "D:/Niklas/repos/aoc2022/data/input5.txt"

parse :: String -> (State, [Move])
parse input = (parseDiagram diagram, map parseMove $ filter (/= "") moves)
  where
    (diagram, moves) = break (== "") $ lines input

parseDiagram xs = IM.fromList $ map (\(x:xs) -> (read [x], filter isAlphaNum $ reverse xs)) 
      $ filter (\line -> isDigit (head line)) $ map reverse $ transpose xs

parseMove cmd = let ws = words cmd in (read (ws !! 1), read (ws !! 3), read (ws !! 5))

performMove state (n, from, to) = IM.adjust (reverse stack ++) to $ IM.adjust (drop n) from state 
    where 
      stack = take n $ state IM.! from

performMove2 state (n, from, to) = IM.adjust (stack ++) to $ IM.adjust (drop n) from state 
    where 
      stack = take n $ state IM.! from
