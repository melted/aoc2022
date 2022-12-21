{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack)
import Data.Attoparsec.Text
import Control.Applicative
import qualified Data.Map as M
import Debug.Trace

inputData = pack <$> readFile "D:/Niklas/repos/aoc2022/data/input21.txt"

data Op = Add | Sub | Mul | Div deriving (Show, Eq)
data Expr = Num Int | Calc Op String String deriving (Show, Eq)

op '*' = Mul
op '+' = Add
op '-' = Sub
op '/' = Div

eval m (Num x) = x
eval m (Calc Add a b) = eval m (m M.! a) + eval m (m M.! b)
eval m (Calc Mul a b) = eval m (m M.! a) * eval m (m M.! b)
eval m (Calc Sub a b) = eval m (m M.! a) - eval m (m M.! b)
eval m (Calc Div a b) = eval m (m M.! a) `div` eval m (m M.! b)

main = do 
    input <- inputData
    let monkeys = M.fromList $ parseAll input
    let res = eval monkeys $ monkeys M.! "root"
    print res
    let Calc _ a b = monkeys M.! "root"
    let res2 = find monkeys (monkeys M.! a) (monkeys M.! b)
    print res2 

find monkeys a b = go 0 10000000000000
    where 
        update x = M.insert "humn" (Num x) monkeys
        val x = eval (update x) a - eval (update x) b
        go x y | val x == 0 = x
        go x y | val y == 0 = y
        go x y | signum (val x) /= signum (val y) = let z = (x+y) `div` 2 in 
                                                        if signum (val z) == signum (val y)
                                                            then go x z
                                                            else go z y
        go _ _ = error "can't bisect"
 
parseMonkey :: Parser (String, Expr)
parseMonkey = do
    name <- many letter
    string ": "
    expr <- parseExpr
    return (name, expr)

parseExpr = Num <$> decimal <|> parseCalc
parseCalc = do
    a <- many letter
    space
    opChar <- satisfy $ inClass "-*+/"
    space
    b <- many letter
    return $ Calc (op opChar) a b

parseAll input = case parseOnly (sepBy parseMonkey endOfLine) input of
                    Left err -> error err
                    Right x -> x