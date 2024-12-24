module AOC2024.Day24
  ( part1,
    part2,
  )
where

import Data.Bits (xor, (.&.), (.|.))
import Data.List (isPrefixOf)
import Data.Map qualified as M
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = zGatesToDec $ processGates memo gates
  where
    (memo, gates) = parseWiresAndGates input

part2 :: T.Text -> Int
part2 input = 0

type Label = String

type Memo = M.Map Label Bool

newtype Input = Input Label deriving (Show)

newtype Output = Output Label deriving (Show)

data Gate
  = AND Input Input Output
  | OR Input Input Output
  | XOR Input Input Output
  deriving (Show)

type Gates = [Gate]

zGatesToDec :: Memo -> Int
zGatesToDec = M.foldlWithKey accFn 0
  where
    accFn :: Int -> Label -> Bool -> Int
    accFn acc k v =
      if ("z" `isPrefixOf` k) && v
        then acc + (2 ^ (read (drop 1 k) :: Int))
        else acc

processGates :: Memo -> Gates -> Memo
processGates memo [] = memo
processGates memo (g : gs) =
  if M.member label1 memo && M.member label2 memo
    then processGates (processGate memo g) gs
    else processGates memo $ gs ++ [g]
  where
    (label1, label2) = case g of
      AND (Input w1) (Input w2) _ -> (w1, w2)
      OR (Input w1) (Input w2) _ -> (w1, w2)
      XOR (Input w1) (Input w2) _ -> (w1, w2)

processGate :: Memo -> Gate -> Memo
processGate memo (AND (Input w1) (Input w2) (Output w3)) = M.insert w3 (memo M.! w1 .&. memo M.! w2) memo
processGate memo (OR (Input w1) (Input w2) (Output w3)) = M.insert w3 (memo M.! w1 .|. memo M.! w2) memo
processGate memo (XOR (Input w1) (Input w2) (Output w3)) = M.insert w3 (memo M.! w1 `xor` memo M.! w2) memo

parseWiresAndGates :: T.Text -> (Memo, Gates)
parseWiresAndGates input = parseAoCInput input wiresAndGatesParser "wiresAndGatesParser"
  where
    boolParser = P.choice [True <$ P.string "1", False <$ P.string "0"]
    wireParser = (,) <$> (P.many1 P.alphaNum <* P.string ": ") <*> boolParser
    gParser g tag =
      g
        <$> (Input <$> P.many1 P.alphaNum <* P.string tag)
        <*> (Input <$> P.many1 P.alphaNum <* P.string " -> ")
        <*> (Output <$> P.many1 P.alphaNum)
    gateParser =
      P.choice
        [ P.try $ gParser XOR " XOR ",
          P.try $ gParser AND " AND ",
          P.try $ gParser OR " OR "
        ]
    gatesParser = P.many1 $ gateParser <* P.optional P.newline
    wiresParser = P.many1 $ wireParser <* P.optional P.newline
    memoParser = M.fromList <$> wiresParser
    wiresAndGatesParser = (,) <$> (memoParser <* P.optional P.newline) <*> gatesParser
