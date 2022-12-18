{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, Text)
import Data.Attoparsec.Text as AP
import Data.Maybe
import Data.List
import Data.Char
import Debug.Trace
import Data.Ord
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S

main = do
    input <- inputData
    let parsed = M.fromList $ parseAll input
    let nodes = M.keys parsed
    let nodesFlow = M.keysSet $ M.filter ((> 0) . fst) parsed
    let dists = M.fromList [((a,b), fromJust $ dist parsed a b)| a <- nodes, b <- nodes]
    let result = solve parsed dists (pack "AA") (M.keysSet parsed)
    print $  result
   -- print $ scores

solve m dists start nodes = go start S.empty nodes 30 0
    where
        go start done left time score | time < 0 = score
        go start done left time score | S.null left = score
        go start done left time score = maximum spawn
            where
                (flow, neighbors) = m M.! start
                spawn = map subgo $ neighbors
                nt = time - min 1 flow
                newScore = if S.member start done then score else flow*nt+score
                subgo l = go l (S.insert start done) (S.delete l left) (nt-1) newScore

inputData = pack <$> readFile "D:/Niklas/repos/aoc2022/data/input16.txt"

parseLine :: Parser (Text, (Int, [Text]))
parseLine = do
    string "Valve "
    valve <- AP.takeWhile isAlpha
    string " has flow rate="
    flow <- decimal
    string "; tunnels lead to valves " <|> string "; tunnel leads to valve "
    conns <- sepBy1 (AP.takeWhile isAlpha) (string ", ")
    return (valve, (flow, conns))
 
parseAll input = case parseOnly (sepBy parseLine endOfLine) input of
                    Left err -> error err
                    Right v -> v

dist :: M.Map Text (Int, [Text]) -> Text -> Text -> Maybe Int
dist m a b = go a b S.empty
    where
        go a b _ | a == b = Just 0
        go a b checked = if null alts then Nothing else Just $ 1 + minimum alts 
            where
                conns = filter (\c -> not $ S.member c checked) (snd $ m M.! a)
                newChecked = S.union checked $ S.fromList conns
                alts = mapMaybe (\c -> go c b newChecked) conns
