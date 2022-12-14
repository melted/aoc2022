{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text hiding (I)
import Data.Text (pack)
import Control.Applicative
import Data.List
import Data.Maybe

inputData = pack <$> readFile "D:/Niklas/repos/aoc2022/data/input13.txt"

main = do
    input <- inputData
    let grouped = parseAll input
    let indexed = zip [1..] grouped 
    let result = sum $ map fst $ filter (\(_,(a,b)) -> a <= b) indexed
    print result
    let (l, r) = unzip grouped
    let packets = l++r++[indicator2, indicator6]
    let sorted = sort packets
    let ix2 = fromMaybe (-1) $ findIndex (== indicator2) sorted
    let ix6 = fromMaybe (-1) $ findIndex (== indicator6) sorted
    print $ (ix2+1)*(ix6+1)

indicator2 = L [L [I 2]]
indicator6 = L [L [I 6]]

data List = I Int | L [List] deriving (Show, Eq)

instance Ord List where
    compare (I a) (I b) = compare a b
    compare (I a) b = compare (L [I a]) b
    compare a (I b) = compare a (L [I b])
    compare (L []) (L []) = EQ
    compare (L []) (L _) = LT
    compare (L _) (L []) = GT
    compare (L (x:xs)) (L (y:ys)) = case compare x y of
                                        EQ -> compare (L xs) (L ys)
                                        z -> z

parseList :: Parser List
parseList = do
    char '['
    vals <- sepBy (parseList <|> (I <$> decimal)) (char ',') 
    char ']'
    return $ L vals

parseGroup = do
    optional endOfLine
    l <- parseList
    endOfLine
    r <- parseList
    endOfLine
    return (l, r)

parseAll input = case parseOnly (many parseGroup) input of
                    Left err -> error err
                    Right output -> output
