module HusyHox.Text.Join (main, join) where

import HusyHox.Common.Extra (readInput)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness join

data JoinSwitch = CheckOrder | NoCheckOrder
    deriving (Bounded, Enum, Eq, Ord, Show)

type IOArgs = (String, String)

type CoreArgs = IOArgs

type FormatArgs = IOArgs

data OutputArgs = OA String

join :: Utility JoinSwitch IOArgs CoreArgs FormatArgs OutputArgs
join = Utility "join" argDescs input parse core format output where
    input args = do
        text1 <- readRequired Unnamed
        text2 <- readRequired Unnamed2
        return (text1, text2)
        where readRequired = readInput . Just . getRequiredArg args . Std

    parse _ = id

    core = id

    format (t1, t2) = OA (t1 ++ t2)

    output (OA text) = putStr text

argDescs :: [Arg (Switch JoinSwitch)]
argDescs =
    [ mkAbbrOnlyData Al "file-number"
          ("Print a line for each unpairable line in file 1 or 2 " ++
           "in addition to the normal output.")
    , mkExt CheckOrder "check-order"
          "Fail if either file is incorrectly ordered" 
    , mkExt NoCheckOrder "nocheck-order"
          "Do not fail if either file is incorrectly ordered (Default)."
    , mkAbbrOnlyData El "string"
          "Replace output fields that are missing in the input with string."
    , mkNamed Il "ignore-case"
          "Ignore differences in case when comparing keys."
    , mkAbbrOnlyData One "field" "Join on field (a positive integer) of file 1."
    , mkAbbrOnlyData Two "field" "Join on field (a positive integer) of file 2."
    , mkAbbrOnlyData Jl  "field" "Equivalent to '-1 field -2 field'"
    , mkAbbrOnlyData Ol  "field-list"
          "Construct each output line according to the format in 'field-list."
    , mkAbbrOnlyData Tl  "char"
          "Use character 'char' as the input and output field separator."
    , mkAbbrOnlyData Vl "file-number"
          ("Print a line for each unpairable line in file 1 or 2 " ++
           "instead of the normal output.")
    , mkHelp
    , mkVersion
    , mkRequiredUnnamed Unnamed  "File1" "File1"
    , mkRequiredUnnamed Unnamed2 "File2" "File2"
    ]
