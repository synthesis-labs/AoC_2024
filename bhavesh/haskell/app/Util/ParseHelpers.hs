module Util.ParseHelpers
  ( parseAoCInput,
  )
where

import Data.Text qualified as T
import Text.Parsec (Parsec, runParser)

parseAoCInput :: T.Text -> Parsec T.Text () a -> String -> a
parseAoCInput input p name = either errorHandler id parseResult
  where
    parseResult = runParser p () name input
    errorHandler = error . show