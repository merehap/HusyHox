module HusyHox.Text.Head (main, head) where

import Prelude hiding (head)

import HusyHox.Common.Extra (readUnnamedArgs)
import HusyHox.Common.Format (fullUnlines)
import HusyHox.Common.Utility
import HusyHox.Common.Text (getCountAndSign, lineApply)

main :: IO ()
main = runUtilityHarness head

defaultCount :: Int
defaultCount = 10

defaultSign :: Bool
defaultSign = True

data HeadSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA Int Bool [[String]]

data FormatArgs = FA [[String]]

data OutputArgs = OA String

head :: Utility HeadSwitch IOArgs CoreArgs FormatArgs OutputArgs
head = Utility "head" argDescs input parse core format output where
    input = fmap IA . readUnnamedArgs

    parse args (IA texts) = CA count fromFront lns
        where (count, fromFront) =
                  getCountAndSign defaultCount defaultSign (Std Nl) args
              lns = map lines texts

    core (CA count front texts) = FA $ lineApply front take ginit count texts

    format (FA texts) = OA $ fullUnlines texts

    output (OA text) = putStr text

argDescs :: [Arg (Switch HeadSwitch)]
argDescs =
    [ mkNamedData Cl "bytes" "K" $ unlines
          [ "print the first K bytes of each file;"
          , "  with the leading `-', print all but the last"
          , "  K lines of each files"
          ]
    , mkNamedData Nl "lines" "K" $ unlines
          [ "print the first K lines instead of the first 10;"
          , "with the leading '-', print all but the last K lines of each file"
          ]
    , mkNamed Ql "quiet" "never print headers giving file names"
    , mkNamed Vl "verbose" "always print headers giving file names"
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

ginit :: Int -> [a] -> [a]
ginit n xs = take (length xs - n) xs
