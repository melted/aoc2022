import qualified Data.Set as S
import Data.Text (pack)
import Data.Attoparsec.Text as AP

inputData = pack <$> readFile "D:/Niklas/repos/aoc2022/data/input18.txt"

main = do
    input <- inputData
    let cubes = S.fromList $ parseAll input
    let result = sum $ map (countFreeSides cubes) $ S.toList cubes
    print result
    let filledOutside = fillExterior (0,0,0) cubes
    let inside = sum $ map (countFreeSides (S.union cubes filledOutside)) $ S.toList cubes
    print $ result-inside

countFreeSides cubes pos = length $ freeSides cubes pos

freeSides cubes pos = S.filter (not . (`S.member` cubes)) $ neighbors pos
neighbors (x,y,z) = S.fromList [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]

overflow (x, y, z) = any (\x -> x < -10 || x > 27) [x,y,z]

fillExterior start cubes = go (S.singleton start) S.empty
    where
        go x done | S.null x = done
        go x done = go (next x) nextDone
            where 
                nextDone = S.union x done 
                next x = (S.filter (not . overflow) $ S.unions $ S.map (freeSides cubes) x) S.\\ nextDone

parseLine :: Parser (Int, Int, Int)
parseLine = do
    a <- decimal
    char ','
    b <- decimal
    char ','
    c <- decimal
    return (a,b,c)

parseAll input = case parseOnly (sepBy parseLine endOfLine) input of
                    Left err -> error err
                    Right x -> x