module Advent (split) where


split :: (Eq a) => a -> [a] -> [[a]]
split sep = foldr check []
    where
        check x acc | x == sep = []:acc
        check x (a:acc) = (x:a):acc
        check x [] = [[x]]
