import qualified Data.Map as M
import Data.Ord
import Data.List

data Monkey = Monkey {
    items :: [Int],
    op :: Int -> Int,
    test :: (Int, Int, Int),
    inspected :: Int
}

instance Show Monkey where
    show (Monkey { items = i, inspected = c }) = "Monkey(items: "
                        ++ show i ++ ", inspected: " ++ show c ++ ")"

main = do
    let result1 = head $ drop 20 $ iterate (playRound True) input
    print $ score result1
    let result = head $ drop 10000 $ iterate (playRound False) input
    print $ score result


score monkeys = product $ take 2 $ sortBy (comparing negate) $ map inspected $ M.elems monkeys

playRound part1 monkeys = foldl (handleMonkey part1) monkeys [0..(length monkeys)-1]

handleMonkey part1 monkeys n = updatedMonkeys
    where
        monkey = monkeys M.! n
        dist = map (handleItem monkey) (items monkey)
        newMonkeys = M.insert n monkey { items = [], inspected = inspected monkey + length dist } monkeys
        give monkeys (n, v) = M.adjust (\m -> m { items = (items m) ++ [v]}) n monkeys
        updatedMonkeys = foldl give newMonkeys dist
        modval = product $ fmap ((\(p,_,_) -> p) . test) (M.elems monkeys)
        handleItem monkey item = (target, newWorry)
            where
                newWorry = (if part1 then (`div` 3) else (`mod` modval)) ((op monkey) item)
                target = let (p, t, e) = test monkey in if newWorry `mod` p == 0 then t else e

input = M.fromList [
    (0, Monkey { items = [54, 98, 50, 94, 69, 62, 53, 85], op = (*13), test = (3, 2, 1), inspected = 0 }),
    (1, Monkey { items = [71, 55, 82], op = (+2), test = (13, 7, 2), inspected = 0 }),
    (2, Monkey { items = [77, 73, 86, 72, 87], op = (+8), test = (19, 4, 7), inspected = 0 }),
    (3, Monkey { items = [97, 91], op = (+1), test = (17, 6, 5), inspected = 0 }),
    (4, Monkey { items = [78,97,51,85,66,63,62], op = (*17), test = (5, 6, 3), inspected = 0 }),
    (5, Monkey { items = [88], op = (+3), test = (7, 1, 0), inspected = 0 }),
    (6, Monkey { items = [87, 57, 63, 86, 87, 53], op = \x->x*x, test = (11, 5, 0), inspected = 0 }),
    (7, Monkey { items = [73, 59, 82, 65], op = (+6), test = (2, 4, 3), inspected = 0 })]

testInput = M.fromList [
    (0, Monkey { items = [79,98], op = (*19), test = (23, 2, 3), inspected = 0 }),
    (1, Monkey { items = [54,65,75,74], op = (+6), test = (19, 2, 0), inspected = 0 }),
    (2, Monkey { items = [79, 60,97], op = \x->x*x, test = (13, 1, 3), inspected = 0 }),
    (3, Monkey { items = [74], op = (+3), test = (17, 0, 1), inspected = 0 })]