module HusyHox.Text.Tac (main, tac) where

import Control.Arrow (second)
import Data.List.Split (keepDelimsL, keepDelimsR, onSublist, split)
import Text.Regex (matchRegexAll, mkRegex)

import HusyHox.Common.Extra (getStdArgOrDefault, if', readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness tac

data TacSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA String Bool Bool [String]

data FormatArgs = FA [String]

data OutputArgs = OA String

tac :: Utility TacSwitch IOArgs CoreArgs FormatArgs OutputArgs
tac = Utility "tac" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse args (IA texts) = CA separator isRegex isBefore texts
        where separator = getStdArgOrDefault "\n" args Sl
              [isRegex, isBefore] = map (gotStdArg args) [Rl, Bl]

    core (CA separator isRegex isBefore texts) = FA $
        map (concat . reverse . splitter isBefore separator) texts
        where splitter = if' isRegex regexSplit textSplit

    format (FA lns) = OA $ concat lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch TacSwitch)]
argDescs =
    [ mkNamed Bl "before"
         ("The separator is attached to the beginning of the record " ++
          "that it precedes in the file.")
    , mkNamed Rl "regex" "Treat the separator as a regular expression."
    , mkNamedData Sl "separator" "separator"
          "Use 'separator' as the record separator instead of newline."
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "file" "file"
    ]

-- Helpers --------------------------------------------------------------------

textSplit :: Bool -> String -> String -> [String]
textSplit isBefore separator = split (keepDelims $ onSublist separator)
    where keepDelims = if' isBefore keepDelimsL keepDelimsR

regexSplit :: Bool -> String -> String -> [String]
regexSplit isBefore regexString target = zipWith (++) fs ss
    where (fs, ss) = if' isBefore (second ([]:)) id . strain $
              segment target
          regex = mkRegex regexString
          segment text = case matchRegexAll regex text of
              Nothing -> [text]
              Just (current, separator, rest, _) ->
                  current : separator : segment rest

strain :: [String] -> ([String], [String])
strain []       = ([], [])
strain [x]      = ([x], [])
strain (x:y:zs) = (x : xr, y : yr)
    where (xr, yr) = strain zs
