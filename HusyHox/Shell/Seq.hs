module HusyHox.Shell.Seq (main, seq) where

import Prelude hiding (seq)

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness seq

data SeqSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data CoreArgs = CA Bool String Integer Integer Integer

data FormatArgs = FA String [String]

data OutputArgs = OA String

seq :: Utility SeqSwitch IOArgs CoreArgs FormatArgs OutputArgs
seq = Utility "seq" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = CA fixedWidth separator start inc end
        where fixedWidth = gotStdArg args Wl
              one = getRequiredStdArg args Unnamed
              [maybeSeparator, two, three] =
                  map (getStdArg args) [Sl, Unnamed2, Unnamed3]
              separator  = fromMaybe "\n" maybeSeparator
              (start, inc, end) =
                  getStepValues (read one) optionalValues
              optionalValues = two >>= \t -> return (read t, fmap read three)

    core (CA fixedWidth separator start increment end) =
        FA separator (if' fixedWidth (map pad) id series)
        where series = map show $ getSeries start increment end
              pad n = replicate (maxLength - length n) '0' ++ n
              maxLength = if' (null series) 0 (length $ last series)

    format (FA sep xs) = OA (intercalate sep xs ++ sep)

    output (OA text) = putStr text

argDescs :: [Arg (Switch SeqSwitch)]
argDescs =
    [ mkNamedData Fl       "format"    "FORMAT"
          "use printf style floating-point FORMAT"
    , mkNamedData Sl       "separator" "STRING"
          "use STRING to separate numbers (default \n)"
    , mkNamed Wl "equal-width"
          "equalize width by padding with leading zeroes"
    , mkHelp
    , mkVersion
    , mkRequiredUnnamed Unnamed  "FIRST"     "FIRST"
    , mkUnnamed         Unnamed2 "INCREMENT" "INCREMENT"
    , mkUnnamed         Unnamed3 "LAST"      "LAST"
    ]

-- Helpers --------------------------------------------------------------------

getStepValues ::
    Integer -> Maybe (Integer, Maybe Integer) -> (Integer, Integer, Integer)
getStepValues end   Nothing                = (1    ,   1, end)
getStepValues start (Just (end, Nothing )) = (start,   1, end)
getStepValues start (Just (inc, Just end)) = (start, inc, end)

getSeries :: Integer -> Integer -> Integer -> [Integer]
getSeries start increment end = [start, start + increment .. end]
