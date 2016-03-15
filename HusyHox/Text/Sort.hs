module HusyHox.Text.Sort (main, sort) where

import qualified Data.List as L (sort)

import HusyHox.Common.Extra (readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness sort

data SortSwitch = RandomSource | Sort | BatchSize | CompressProgram | Files0From
    deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA [[String]]

data FormatArgs = FA [[String]]

data OutputArgs = OA String

sort :: Utility SortSwitch IOArgs CoreArgs FormatArgs OutputArgs
sort = Utility "sort" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse _ (IA texts) = CA $ map lines texts

    core (CA texts) = FA $ map L.sort texts

    format (FA lns) = OA $ concatMap unlines lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch SortSwitch)]
argDescs =
    [ mkNamed Bl "ignore-leading-blanks" "ignore leading blanks"
    , mkNamed Dl "dictionary-order"
          "consider only blanks and alphanumeric characters"
    , mkNamed Fl "ignore-case" "fold lower case to upper case characters"
    , mkNamed Gl "general-numeric-sort"
          "compare according to general numerical value"
    , mkNamed Il "ignore-nonprinting" "consider only printable characters"
    , mkNamed M  "month-sort" "compare (unknown) < `JAN' < ... < `DEC'"
    , mkNamed Hl "human-numeric-sort"
          "compare human readable numbers (e.g., 2K 1G)"
    , mkNamed Nl "numeric-sort" "compare according to string numerical value"
    , mkNamed R  "random-sort" "sort by random hash of keys"
    , mkExtData RandomSource "random-source" "FILE"
          "get random bytes from FILE"
    , mkNamed Rl "reverse" "reverse the result of comparisons"
    , mkExtData Sort "sort" "WORD"
          ("sort according to WORD:\n" ++
           "general-numeric -g, human-numeric -h, month -M,\n" ++
           "random -R, version -V")
    , mkNamed V  "version-sort" "natural sort of (version) numbers within text"
    , mkExtData BatchSize "batch-size" "NMERGE"
          ("merge at most NMERGE inputs at once;\n" ++
           "for more use temp files")
    , mkNamedData Cl "check" "diagnose-first"
          "check for sorted input; do not sort"
    , mkNamedData C  "check" "quiet" "check for sorted input; do not sort"
    , mkExtData CompressProgram "compress-program" "PROG"
          ("compress temporaries with PROG;" ++
           "decompress them with PROG -d")
    , mkExtData Files0From "files0-from" "F"
          ("read input from the files specified by\n" ++
           "NUL-terminated names in file F;\n" ++
           "If F is - then read names from standard input")
    , mkNamedData Kl "key" "POS1[,POS2]"
          ("start a key at POS1 (origin 1), end it at POS2\n" ++
           "(default end of line)")
    , mkNamed Ml "merge" "merge already sorted files; do not sort"
    , mkNamedData Ol "output" "FILE"
          "write result to FILE instead of standard output"
    , mkNamed Sl "stable" "stabilize sort by disabling last-resort comparison"
    , mkNamedData S  "buffer-size" "SIZE" "use SIZE for main memory buffer"
    , mkNamedData Tl "field-separator" "SEP"
          "use SEP instead of non-blank to blank transition"
    , mkNamedData T "temporary-directory" "DIR"
          ("use DIR for temporaries, not $TMPDIR or /tmp;\n" ++
           "multiple options specify multiple directories\n")
    , mkNamed Ul "unique"
          ("with -c, check for strict ordering;\n" ++
           "without -c, output only the first of an equal run")
    , mkNamed Zl "zero-terminated" "end lines with 0 byte, not newline"
    , mkHelp
    , mkVersion
    ]
