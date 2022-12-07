import Data.Text
import Data.Attoparsec.Text as AP

data Cmd = Cd Target | Ls [(Text, Int)]

data Target = Root | Up | Down Text

data Entry = Dir Text [Entry] | File Text Int

inputData = pack <$> readFile "D:/Niklas/repos/aoc2022/data/input7.txt"

main = do
    input <- inputData
    print input

parseInput = many' parseCommand

parseCommand = AP.takeWhile1 (/= '\n') 