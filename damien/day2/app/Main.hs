{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Text.Parsec (runParser, many1, digit, newline, char, Parsec, sepBy, choice, optional, optionMaybe)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Token (GenTokenParser(symbol))
import Data.List (foldr)

data IncDec = Inc
            | Dec 
            | Unknown
            | Broke 
            deriving Eq

main :: IO ()
main = do
    text <- readFile "input2.txt"
    let parsed = run parseLines text

    print $ part1 parsed

number :: Parsec String () [Char]
number = do
    num <- many1 digit
    _ <- optionMaybe $ char ' '
    pure num

line :: Parsec String () [Int]
line = do
    numbers <- many1 number
    let actualNumbers = read <$> numbers
    pure actualNumbers

parseLines :: Parsec String () [[Int]]
parseLines = many1 $ line <* newline

run :: Parsec String () a -> String -> a
run p input =
    case runParser p () "input" input of
        Left err -> error $ show err
        Right a -> a
    
-- part1 :: [[Int]] -> Int
part1 = sum <$> map checkListMonotonic

checkListMonotonic :: [Int] -> Int
checkListMonotonic list = let (_, incDec) = foldl checkElementsMonotonic (head list, Unknown)  $ tail list
                     in if incDec == Broke then 
                        0
                    else 1

checkElementsMonotonic :: (Int, IncDec) -> Int ->  (Int, IncDec)
checkElementsMonotonic (prev, acc)  curr 
    = case acc of
        Broke -> (curr, Broke)
        Unknown -> (curr, checkSingleElem curr prev)
        Inc -> notEqual curr prev Inc
        Dec -> notEqual curr prev Dec
        
notEqual :: Int -> Int -> IncDec -> (Int, IncDec)
notEqual curr prev acc = 
    if checkSingleElem curr prev == acc then
        (curr, acc)
    else 
        (curr, Broke)
        
checkSingleElem :: Int -> Int -> IncDec
checkSingleElem curr prev = 
    case curr - prev of                                
         x | abs x > 3  || abs x < 1 -> Broke
         x | x < 0 -> Dec
         x | x > 0 -> Inc
         _ -> error "This should not happen"

                     


