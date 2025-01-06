module AOC2024.Day22
  ( part1,
    part2,
  )
where

import Data.Bits (Bits (xor))
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum secretNumbers2000
  where
    secretNumbers2000 = V.last . generateNSecrets V.empty 2000 <$> secretNumbers
    secretNumbers = parseSecretNumbers input

part2 :: T.Text -> Int
part2 input = 0

data Op = Mix | Prune deriving (Show, Eq, Ord)

type SecretNumbers = V.Vector Int

generateNSecrets :: SecretNumbers -> Int -> Int -> SecretNumbers
generateNSecrets acc 0 _ = acc
generateNSecrets acc n secretNumber = generateNSecrets (acc V.++ V.singleton newSecret) (n - 1) newSecret
  where
    newSecret = generateNewSecretNumber secretNumber

generateNewSecretNumber :: Int -> Int
generateNewSecretNumber secretNumber = mixOrPrune Prune 0 val3
  where
    val = mixOrPrune Mix (secretNumber * 64) secretNumber
    step1Result = mixOrPrune Prune 0 val
    val2 = mixOrPrune Mix (step1Result `div` 32) step1Result
    step2Result = mixOrPrune Prune 0 val2
    val3 = mixOrPrune Mix (step2Result * 2048) step2Result

mixOrPrune :: Op -> Int -> Int -> Int
mixOrPrune Mix val secretNumber = val `xor` secretNumber
mixOrPrune Prune _ secretNumber = secretNumber `mod` 16777216

parseSecretNumbers :: T.Text -> SecretNumbers
parseSecretNumbers input = parseAoCInput input secretNumbersParser "secretNumbersParser"
  where
    secretNumberParser = read <$> P.many1 P.digit
    secretNumbersParser = V.fromList <$> P.many1 (secretNumberParser <* P.optional P.newline)
