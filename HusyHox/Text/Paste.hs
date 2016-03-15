module HusyHox.Text.Paste (main, paste) where

import Data.List (intercalate, transpose)

import HusyHox.Common.Extra (readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness paste

data PasteSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA Bool [[String]]

data FormatArgs = FA [[String]]

data OutputArgs = OA String

paste :: Utility PasteSwitch IOArgs CoreArgs FormatArgs OutputArgs
paste = Utility "paste" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse args (IA texts) = CA serial lns
        where serial = gotStdArg args Sl
              lns    = map lines texts

    core (CA _ xs) = FA $ transpose xs

    format (FA lns) = OA $ unlines $ map (intercalate "\t") lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch PasteSwitch)]
argDescs =
    [ mkNamedData Dl "delimiters" "LIST"
          "reuse characters from LIST instead of TABS"
    , mkNamed Sl "serial" "paste one file at a time instead of in parallel"
    , mkHelp
    , mkVersion
    ]
