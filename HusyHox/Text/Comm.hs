module HusyHox.Text.Comm (main, comm) where

import Control.Monad (liftM2)

import HusyHox.Common.Extra (readInput)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness comm

data CommSwitch = CheckOrder | NoCheckOrder | OutputDelimiter
    deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String String

data CoreArgs = CA String String

data FormatArgs = FA String

data OutputArgs = OA String

comm :: Utility CommSwitch IOArgs CoreArgs FormatArgs OutputArgs
comm = Utility "comm" argDescs input parse core format output where
    input args = liftM2 IA (readFileFromArg Unnamed) (readFileFromArg Unnamed2)
        where readFileFromArg = readInput . Just . getRequiredArg args . Std

    parse _ (IA text1 text2) =  CA text1 text2

    core (CA text1 text2) = FA (text1 ++ text2)

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch CommSwitch)]
argDescs =
    [ mkAbbrOnly One   "suppress column 1 (lines unique to FILE1)"
    , mkAbbrOnly Two   "suppress column 2 (lines unique to FILE2)"
    , mkAbbrOnly Three "suppress column 3 (lines that appear in both files"
    , mkExt      CheckOrder "check-order"
          ("check that the input is correctly sorted," ++
           "even if all input lines are sorted")
    , mkExt      NoCheckOrder "no-check-order"
          "do not check that the input is correctly sorted"
    , mkExtData OutputDelimiter "output-delimiter" "STR"
          "separate columns with STR"
    , mkHelp
    , mkVersion
    , mkRequiredUnnamed Unnamed "FILE1" "FILE1"
    , mkRequiredUnnamed Unnamed2 "FILE2" "FILE2"
    ]
