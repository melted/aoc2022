import Data.Char
import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Set as S

inputData = readFile "D:/Niklas/repos/aoc2022/data/input19.txt"

data Blueprint = Blueprint {
    bpid :: Int,
    oreCost :: Int,
    clayCost :: Int,
    obsidianCost :: (Int, Int),
    geodeCost :: (Int, Int)
} deriving (Show, Eq, Ord)

data Materials = Mat {
    geode :: Int,
    obsidian :: Int,
    clay :: Int,
    ore :: Int
} deriving (Show, Eq, Ord)

startPile = Mat 0 0 0 0
startRobots = Mat 0 0 0 1

main = do 
    input <- inputData
    let blueprints = map parseLine $ filter (/= "") $ lines input
    let result = map (\b -> (bpid b, run b 24 (S.singleton (startPile, startRobots)))) blueprints
    print $ sum $ map (\(i, (p, r)) -> i * (geode p)) result
    let bp2 = take 3 blueprints
    let res = product $ map (\b -> geode $ fst $ run b 32 (S.singleton (startPile, startRobots))) bp2
    print res

work pile robots = Mat {
    ore = (ore pile) + (ore robots),
    clay = (clay pile) + (clay robots),
    obsidian = (obsidian pile) + (obsidian robots),
    geode = (geode pile) + (geode robots)
}

run bp 0 sols = maximum $ S.toList $ sols
run bp minute states = run bp (minute-1) newStates
    where
        buyOre (pile, robots) = if ore pile >= (oreCost bp) && 
                                   ore robots < max (clayCost bp) (max (fst $ obsidianCost bp) (fst $ geodeCost bp)) 
                                    then Just (pile { ore = ore pile - oreCost bp }, robots { ore = ore robots + 1}) 
                                    else Nothing
        buyClay (pile, robots)= if ore pile >= (clayCost bp) &&
                                   clay robots < snd (obsidianCost bp)
                                   then Just (pile { ore = ore pile - clayCost bp }, robots { clay = clay robots + 1}) 
                                   else Nothing
        buyObsidian (pile, robots) = if ore pile >= fst (obsidianCost bp) && 
                                        clay pile >= snd (obsidianCost bp) &&
                                        obsidian robots < snd (geodeCost bp)
                                        then Just (pile { ore = (ore pile) - fst (obsidianCost bp), 
                                                   clay = clay pile - snd (obsidianCost bp) }, 
                                                   robots { obsidian = obsidian robots + 1}) 
                                        else Nothing
        buyGeode (pile, robots) = if ore pile >= fst (geodeCost bp) && obsidian pile >= snd (geodeCost bp) 
                                    then Just (pile { ore = ore pile - fst (geodeCost bp), 
                                                      obsidian = obsidian pile - snd (geodeCost bp) }, 
                                                      robots { geode = geode robots + 1})
                                    else Nothing
        moves pr  = [pr] ++ mapMaybe (\f -> f pr)[buyClay, buyOre, buyGeode, buyObsidian]
        updateState (pile, robots) = S.fromList $ map (\(p, nr) -> (work p robots, nr)) $ moves (pile,robots)
        newStates = purge  $ S.unions $ S.map updateState states
        hasGeode = maximum $ S.toList $ S.map ( geode .snd) states
        hasObsidian = maximum $ S.toList $ S.map ( obsidian .snd) states
        purge s | hasGeode > 0 = S.filter (\v -> (geode $ snd v) > (hasGeode-2)) s
        purge s | hasObsidian > 0 = S.filter (\v -> (obsidian $ snd v) > (hasObsidian-2)) s
        purge s = s

parseLine str = Blueprint {
                    bpid = id,
                    oreCost = ore,
                    clayCost = clay,
                    obsidianCost = (oo, oc),
                    geodeCost = (go, gob)
                }
    where
        [id, ore, clay, oo, oc, go, gob] = map read $ filter (all isDigit) $
                                                 map (takeWhile (/= ':')) $ words str