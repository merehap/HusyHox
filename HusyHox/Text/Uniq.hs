module HusyHox.Text.Uniq (main, uniq) where

import Control.Arrow ((&&&))
import Data.Char (toLower)
import Data.Function (on)
import Data.List (groupBy, intercalate)

import HusyHox.Common.Extra (choose, if', maybeIf, readInput, writeOutput)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness uniq

data SaveOption
    = Compress  Bool           -- Show one line for each group
    | Empty                    -- Show nothing
    | Repeat    Bool           -- Show one line for each multi-line group
    | RepeatAll (Maybe String) -- Show all lines for each multi-line group
    | Unique    Bool           -- Show one line for each single-line group

data UniqSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA
    SaveOption     -- What lines to save based on uniqueness properties
    (Maybe Int)    -- How many fields to skip per line
    (Maybe Int)    -- How many chars to skip per line
    (Maybe Int)    -- How many chars to check per line
    Bool           -- Whether to ignore case
    Bool           -- Whether to use zero delimiters for the lines
    (Maybe String) -- Maybe the path to which to output
    String         -- The text to process

data FormatArgs = FA (Maybe String) String

data OutputArgs = OA (Maybe String) String

uniq :: Utility UniqSwitch IOArgs CoreArgs FormatArgs OutputArgs
uniq = Utility "uniq" argDescs input parse core format output where
    input = fmap IA . readInput . flip getStdArg Unnamed

    parse args (IA text) = coreArgs
        where [n, r, ra, u, i, z] = map got [Cl, Dl, D, Ul, Il, Zl]
              [f, s, w] = map getInt [Fl, Sl, Wl]
              saveOption = getSaveOption n r ra u
              maybePath  = get Unnamed2
              coreArgs   = maybe
                  (error countAllRepeatedMessage)
                  (\o -> CA o f s w i z maybePath text)
                  saveOption
              getInt = fmap read . get
              get    = getStdArg args
              got    = gotStdArg args

    core (CA so sf sc cc ic zd p text) = FA p $
        ( intercalate (if' zd "\0" "\n")
        . numbered
        . saved so sf sc cc ic
        . lines
        ) text

    format (FA mp t) = OA mp t

    output (OA maybePath text) = writeOutput maybePath text

argDescs :: [Arg (Switch UniqSwitch)]
argDescs =
    [ mkNamedData Fl "skip-fields" "n"
         "Skip 'n' fields on each line before checking for uniqueness."
    , mkNamedData Sl "skip-chars" "n"
          "Skip 'n' characters before checking for uniqueness."
    , mkNamed Cl "count"
          "Print the number of times each line occurred along with the line."
    , mkNamed Il "ignore-case"
          "Ignore case differences when comparing lines."
    , mkNamed Dl "repeated" "Discard lines that are not repeated."
    , mkNamedData D "all-repeated" "delimit-method"
          "Discard lines that are not repeated. Do not discard repeated lines."
    , mkNamed Ul "unique" "Only print unique lines."
    , mkNamedData Wl "check-chars" "n"
          "Compare at most 'n' characters on each line."
    , mkNamed Zl "zero-terminated"
          "Delimit items with a zero byte rather than a newline."
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed  "Input" "Input"
    , mkUnnamed Unnamed2 "Output" "Output"
    ]

-- Parse Helpers --------------------------------------------------------------

countAllRepeatedMessage :: String
countAllRepeatedMessage =
    "printing all duplicated lines and repeat counts is meaningless"

-- Enable/disable count, repeated, all-repeated, unique
getSaveOption :: Bool -> Bool -> Bool -> Bool -> Maybe SaveOption
getSaveOption n = s
    where s True _    True = Nothing
          s _    True True = Just   Empty -- TODO
          s True _    _    = Just $ RepeatAll Nothing
          s _    True _    = Just $ Repeat    n
          s _    _    True = Just $ Unique    n
          s _    _    _    = Just $ Compress  n

-- Core Helpers ---------------------------------------------------------------

numbered :: [(Maybe Int, String)] -> [String]
numbered = map (uncurry (maybe id number))
    where number c v = "\t" ++ show c ++ "\t" ++ v

saved :: SaveOption
      -> Maybe Int
      -> Maybe Int
      -> Maybe Int
      -> Bool
      -> [String]
      -> [(Maybe Int, String)]
saved saveOption skipFields skipChars checkChars ignoreCase =
    flip (.) grouped $ case saveOption of
        Compress  c -> mapLH c
        Empty       -> const []
        Repeat    c -> mapLH c . fs not
        RepeatAll _ -> zipN    . fs not
        Unique    c -> mapLH c . fs id
        where mapLH c = map (maybeIf c length &&& head)
              zipN    = zip (repeat Nothing) . concat
              fs      = filter . (. checkLength 1)
              grouped = groupBy ((==) `on`
                  ( cased ignoreCase
                  . checked checkChars
                  . unskipped skipChars skipFields
                  ))

cased :: Bool -> String -> String
cased = choose (map toLower) id

checked :: Maybe Int -> String -> String
checked = maybe id take

unskipped :: Maybe Int -> Maybe Int -> String -> String
unskipped skipChars skipFields = maybe id sc skipChars . maybe id sf skipFields
    where sf n = unwords . drop n . words
          sc   = drop

checkLength :: Int -> [a] -> Bool
checkLength 0 []     = True
checkLength 0 _      = False
checkLength _ []     = False
checkLength n (_:xs) = checkLength (n - 1) xs
