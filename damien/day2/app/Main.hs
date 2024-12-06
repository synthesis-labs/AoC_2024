{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Text.Parsec (runParser, many1, digit, newline, char, Parsec, sepBy, choice, optional, optionMaybe)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Token (GenTokenParser(symbol))
import Data.List (foldr, sort, group)

data IncDec = Inc
            | Dec
            | Unknown
            | Broke
            deriving (Eq, Show, Ord)

main :: IO ()
main = do
    text <- readFile "input2.txt"
    let parsed = run parseLines text
    print $ length $ filter isIncDecValid $ map createListOfIncDec parsed
    print $ length $ filter id $ map checkOne parsed

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

checkSingleElem :: Int -> Int -> IncDec
checkSingleElem curr prev =
    case curr - prev of
         x | abs x > 3  || abs x < 1 -> Broke
         x | x < 0 -> Dec
         x | x > 0 -> Inc
         _ -> error "This should not happen"

createListOfIncDec :: [Int] -> [(IncDec, Int)]
createListOfIncDec list
    = map (\xs@(x:_) -> (x, length xs))
    $ group
    $ sort
    $ zipWith checkSingleElem list (tail list)

checkWithRemoveNth :: [Int] -> Int -> Bool
checkWithRemoveNth levelList nth = isIncDecValid $ createListOfIncDec $ deleteNth nth levelList

deleteNth :: Int -> [a] -> [a]
deleteNth i xs = take i xs ++ drop (succ i) xs

checkOne :: [Int] -> Bool
checkOne levelList
    = or 
    $ take (length levelList)
    $ map (checkWithRemoveNth levelList) [0..] <> [(\x -> length x == 1) $ createListOfIncDec levelList]

isIncDecValid :: [(IncDec, Int)] -> Bool
isIncDecValid xs = length xs == 1 && (fst (head xs) == Inc || fst (head xs) == Dec)
