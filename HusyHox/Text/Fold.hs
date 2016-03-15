module HusyHox.Text.Fold (main, fold) where

import Data.List (intercalate)
import Data.List.Split (splitEvery)

import HusyHox.Common.Extra (readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness fold

defaultWidth :: Int
defaultWidth = 80

data FoldSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA Int Bool CountType [String]

data FormatArgs = FA [String]

data CountType = Bytes | Chars | Columns

data OutputArgs = OA String

fold :: Utility FoldSwitch IOArgs CoreArgs FormatArgs OutputArgs
fold = Utility "fold" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse args (IA texts) = CA width spaces (getCountType bytes chars) texts
        where width = maybe defaultWidth read $ getStdArg args Wl
              [bytes, chars, spaces] = map (gotStdArg args) [Bl, Cl, Sl]

    core (CA width _ _ texts) = FA $
        map (intercalate "\n" . concatMap (splitEvery width) . lines) texts

    format (FA lns) = OA $ concat lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch FoldSwitch)]
argDescs =
    [ mkNamed Bl "bytes"      "count bytes rather than columns"
    , mkNamed Cl "characters" "count characters rather than columns"
    , mkNamed Sl "spaces"     "break at spaces"
    , mkNamedData Wl "width" "WIDTH" "use WIDTH columns instead of 80"
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

getCountType :: Bool -> Bool -> CountType
getCountType = g
    where g False False = Columns
          g False True  = Chars
          g True  False = Bytes
          g True  True  = error "only one way of folding may be specified"
