{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, Text)
import Data.List (foldl', tails)
import Data.Attoparsec.Text as AP
import Control.Applicative
import qualified Data.Map as M

data Cmd = Cd Target | Ls [Entry] deriving (Show, Eq)

data Target = Root | Up | Down Text deriving (Show, Eq)

data Entry = Dir Text | File Text Int deriving (Show, Eq)

inputData = pack <$> readFile "D:/Niklas/repos/aoc2022/data/input7.txt"

main = do
    input <- inputData
    let cmds = case parseInput input of
                    Right cmd -> cmd
                    Left err -> error err
    let (_, dirs) = foldl' processCommand ([], M.empty) cmds
    print $ sum $ filter (< 100000) $ M.elems dirs
    let total = dirs M.! []
    let need = 30000000
    let remaining = 70000000 - total
    let minSpace = need - remaining
    print $ minimum $ filter (> minSpace) $ M.elems dirs

processCommand (stack, found) (Cd Root) = ([], found)
processCommand (x:xs, found) (Cd Up) = (xs, found)
processCommand (path, found) (Cd (Down dir)) = (dir:path, found)
processCommand (path, found) (Ls entries) = (path, updated)
    where
        updated = foldl' (\m k -> M.insertWith (+) k total m) found (tails path)
        total = sum (map size entries)
        size (File _ s) = s
        size _ = 0

parseInput = parseOnly (many parseCommand)

parseCommand = parseCd <|> parseLs

parseCd = do
    string "$ cd "
    str <- takeTill isEndOfLine
    let target = if str == ".." then Up else if str == "/" then Root else Down str
    end
    return $ Cd target

end = endOfLine <|> endOfInput

parseLs = do
    string "$ ls"
    endOfLine
    files <- sepBy parseEntry endOfLine
    end
    return $ Ls files

parseEntry = parseDir <|> parseFile

parseDir = do
    string "dir "
    name <- takeTill isEndOfLine
    return $ Dir name

parseFile = do
    size <- decimal
    space
    name <- takeTill isEndOfLine
    return $ File name (fromIntegral size)