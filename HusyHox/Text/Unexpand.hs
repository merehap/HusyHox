module HusyHox.Text.Unexpand (main, unexpand) where

import Data.List (intercalate)
import Data.List.Split (splitEvery)

import HusyHox.Common.Extra (readInputs, tabSpaces)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness unexpand

data UnexpandSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA [[String]]

data FormatArgs = FA [[String]]

data OutputArgs = OA String

unexpand :: Utility UnexpandSwitch IOArgs CoreArgs FormatArgs OutputArgs
unexpand = Utility "unexpand" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse _ (IA texts) = CA $ map lines texts

    core (CA lns) = FA $ map (map (replaceInitials tabSpaces "\t")) lns

    format (FA lns) = OA $ concatMap (intercalate "\n") lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch UnexpandSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

replaceInitials :: String -> String -> String -> String
replaceInitials old new text = concat $ replicate (length initials) new ++ rest
    where chunks = splitEvery (length old) text
          (initials, rest) = span (== old) chunks
