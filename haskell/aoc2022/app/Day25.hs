
inputData = readFile "../../data/input25.txt"

main = do 
    input <- inputData
    let nums = map parseSnafu $ lines input
    let res = toSnafu $ sum nums
    print res

val '=' = -2
val '-' = -1
val '0' = 0
val '1' = 1
val '2' = 2

dig (-2) = '='
dig (-1) = '-'
dig 0 = '0'
dig 1 = '1'
dig 2 = '2'

parseSnafu input = foldl (\acc ch -> val ch + acc*5) 0 input

toSnafu n = go n ""
    where
        go 0 acc = acc
        go rest acc = go nr ((dig nd):acc)
            where
                (r, d) = rest `divMod` 5
                (nr, nd) = if d > 2 then (r+1, d-5) else (r,d)