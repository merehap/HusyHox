module HusyHox.Text.Tail (main, tail) where

import Prelude hiding (tail)

import HusyHox.Common.Extra (readUnnamedArgs)
import HusyHox.Common.Format (fullUnlines)
import HusyHox.Common.Text (getCountAndSign, lineApply)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness tail

defaultCount :: Int
defaultCount = 10

defaultSign :: Bool
defaultSign = False

data TailSwitch = MaxUnchangedStats | Pid | Retry
    deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA Int Bool [[String]]

data FormatArgs = FA [[String]]

data OutputArgs = OA String

tail :: Utility TailSwitch IOArgs CoreArgs FormatArgs OutputArgs
tail = Utility "tail" argDescs input parse core format output where
    input = fmap IA . readUnnamedArgs

    parse args (IA texts) = CA count front lns
        where (count, front) =
                  getCountAndSign defaultCount defaultSign (Std Nl) args
              lns = map lines texts

    core (CA count front texts) = FA $ lineApply front drop glast count texts

    format (FA texts) = OA $ fullUnlines texts

    output (OA text) = putStr text

argDescs :: [Arg (Switch TailSwitch)]
argDescs =
    [ mkNamedData Cl "bytes" "K" $ unlines
          [ "output the last K bytes; alternatively, use -c +K"
          , "to output bytes starting with the Kth of each file"
          ]
    , mkNamedData Fl "follow" "name|descriptor" $ unlines
          [ "output appended data as the file grows;"
          , "-f, --follow, and --follow=descriptor are"
          , "equivalent"
          ]
    , mkAbbrOnly F "same as --follow=name --retry"
    , mkNamedData Nl "lines" "K" $ unlines
          [ "output the last K lines instead of the last 10"
          , "or use -n +K lines to output lines starting with the Kth"
          ]
    , mkExtData MaxUnchangedStats "max-unchanged-stats" "N" $ unlines
          [ "with --follow=name, reopen a FILE which has not"
          , "changed size after N (default 5) iterations"
          , "to see if it has been unlinked or renamed"
          , "(this is the usual case of rotated log files)"
          ]
    , mkExtData Pid "pid" "PID"
          "with -f, terminated after process ID, PID dies"
    , mkNamedData Ql "quiet" "silent" "never output headers giving file names"
    , mkExt Retry "retry" $ unlines
          [ "keep trying to open a file even when it is or"
          , "  becomes inaccessible; useful when following by"
          , "  name, i.e., with --follow=name"
          ]
    , mkNamedData Sl "sleep-interval" "N" $ unlines
          [ "with -f, sleep for approximately N seconds"
          , "  (default 1.0) between iterations"
          ]
    , mkNamed Vl "verbose" "always output headers giving file names"
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

glast :: Int -> [a] -> [a]
glast n xs = drop (length xs - n) xs
