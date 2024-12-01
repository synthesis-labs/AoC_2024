module Model
  ( AoCAnswer,
    AoCDay,
    Timing (..),
    Part (Part),
    Parts,
    Year,
    errMsgParts,
  )
where

import Data.Text qualified as T

type Year = Int

type Day = Int

type AoCDay = (Year, Day)

data Part = forall a. (Show a) => Part (T.Text -> a)

type Parts = (Part, Part)

data Timing
  = Value Double
  | NoValue

type AoCAnswer = (String, Timing)

errMsgParts :: forall a. (Show a) => a -> Parts
errMsgParts s = (Part . const $ s, Part . const $ s)