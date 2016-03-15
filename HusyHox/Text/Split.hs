module HusyHox.Text.Split (main, split) where

import Control.Arrow (second)
import Data.List.Split (splitEvery)

import HusyHox.Common.Extra (getStdArgOrDefault, readInput)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness split

data SplitSwitch = Verbose deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA String Int [String]

data FormatArgs = FA [(String, [String])]

data OutputArgs = OA [(String, String)]

split :: Utility SplitSwitch IOArgs CoreArgs FormatArgs OutputArgs
split = Utility "split" argDescs input parse core format output where
    input args = fmap IA $ readInput (getArg args $ Std Unnamed)

    parse args (IA text) = CA prefix linesPerFile lns
        where prefix = getStdArgOrDefault "x" args Unnamed2
              linesPerFile = maybe 1000 read (getArg args $ Std Ll)
              lns = lines text

    core (CA prefix count lns) =
        FA $ zip (map (prefix ++) suffixes) (splitEvery count lns)

    format (FA xs) = OA $ map (second concat) xs

    output (OA xs) = mapM_ (uncurry writeFile) xs

argDescs :: [Arg (Switch SplitSwitch)]
argDescs =
    [ mkNamedData Al "suffix-length" "N" "use suffixes of length N (default 2)"
    , mkNamedData Bl "bytes" "SIZE" "put SIZE bytes per output file"
    , mkNamedData C  "line-bytes" "SIZE"
          "put at most SIZE bytes of lines per output file"
    , mkNamed Dl
          "numeric-suffixes" "use numeric suffixes instead of alphabetic"
    , mkNamedData Ll "lines" "NUMBER" "put NUMBER lines per output file"
    , mkExt Verbose "verbose" "print a diagnostic just before each"
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "INPUT" "INPUT"
    , mkUnnamed Unnamed2 "PREFIX" "PREFIX"
    ]

-- Helpers --------------------------------------------------------------------

suffixes :: [String]
suffixes = zipWith (\x y -> [x,y])
                   (concatMap (replicate 26) ['a'..'z'])
                   (cycle ['a'..'z'])
