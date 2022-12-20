import qualified Data.Sequence as S
import Data.Maybe

inputData = readFile "D:/Niklas/repos/aoc2022/data/input20.txt"

parseData :: String -> [Int]
parseData input =  map read $ lines input

main = do
    input <- inputData
    let msg = parseData input
    let res = decrypt 1 msg
    print $ res
    let msg2 = map (*811589153) msg
    let res2 = decrypt 10 msg2
    print $ res2


decrypt n msg = mix n (S.fromList $ zip [0..] msg)
    where
        index x = x `mod` length msg
        mapping = I.fromList (zip [0..length msg-1] [0..])
        mix 0 seq = sum s
            where
                zero = fromMaybe (error "oh hell") $ S.findIndexR ((== 0) . snd) seq 
                s = map (\x -> snd $ seq `S.index` (index (zero + x))) [1000, 2000, 3000]
        mix n seq = mix (n-1) (go (0, seq))
        go (i, seq) | i == length seq = seq
        go x = go (step x)

step (i, seq) = (i + 1, nseq)
    where
        index x = x `mod` length seq
        ix = fromMaybe (error "oh no") $ S.findIndexL ((== i) . fst) seq
        v = snd $ seq `S.index` ix
        target = (ix+v) `mod` (length seq - 1)
        nseq = if target /= ix 
                then S.insertAt target (i, v) $ S.deleteAt ix seq
                else seq