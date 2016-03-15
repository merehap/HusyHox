module HusyHox.File.Mktemp (main, mktemp) where

import Control.Arrow ((***))
import Control.Monad (unless)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import System.Directory (createDirectory)
import System.FilePath (combine)
import System.Random (StdGen, newStdGen)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Random
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness mktemp

data MktempSwitch = Suffix | Tmpdir deriving (Bounded, Enum, Eq, Ord, Show)

data CoreArgs = CA Bool Bool String String String StdGen

data FormatArgs = FA Bool Bool String

data OutputArgs = OA Bool Bool String

mktemp :: Utility MktempSwitch StdGen CoreArgs FormatArgs OutputArgs
mktemp = Utility "mktemp" argDescs input parse core format output where
    input _ = newStdGen

    parse args = CA isDir dryRun baseDir template suffix
        where [isDir, dryRun] = map (gotArg args . Std) [Dl, Ul]
              suffix = fromSwitch "" (Ext Suffix)
              baseDir = getBaseDirectory args
              template = fromSwitch "tmp.XXXXXXXXXX" (Std Unnamed)
              fromSwitch def = fromMaybe def . getArg args

    core (CA isDir dryRun baseDir template suffix gen) =
        FA isDir dryRun (newTempFilePath baseDir template suffix gen)

    format (FA isDir dryRun path) = OA isDir dryRun path

    output (OA isDir dryRun path) = do
        unless dryRun $ if' isDir createDirectory (`writeFile` "") path
        putStrLn path

argDescs :: [Arg (Switch MktempSwitch)]
argDescs =
    [ mkNamed Dl "directory" "create a directory, not a file"
    , mkNamed Ul "dry-run"
          "do not create anything; merely print a name (unsafe)"
    , mkNamed Ql "quiet" "suppress diagnostics about file/dir-creation failure"
    , mkExtData Suffix "suffix" "SUFF"
          ("append SUFF to TEMPLATE. SUFF must not contain slash.\n" ++
           " This option is implied if TEMPLATE does not end in X.")
    , mkExtData Tmpdir "tmpdir" "DIR"
          ("interpret TEMPLATE relative to DIR. If DIR is not\n" ++
           " specified, use $TMPDIR if set, else /tmp. With\n" ++
           " this option, TEMPLATE mus not be an absolute name.\n" ++
           " Unlike with -t, TEMPLATE may contain slashes, but\n" ++
           " mktemp creates only the final component")
    , mkAbbrOnlyData Pl "DIR" "use DIR as a prefix; implies -t [deprecated]"
    , mkAbbrOnly Tl
          ("interpret TEMPLATE as a single file name component,\n" ++
           " relative to a directory: $TMPDIR, if set; else the\n" ++
           " directory specified via -p; else /tmp [deprecated]")
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "TEMPLATE" "TEMPLATE"
    ]

-- Helpers --------------------------------------------------------------------

getBaseDirectory :: Args (Switch MktempSwitch) -> String
getBaseDirectory args = fromMaybe "/tmp" . listToMaybe $ mapMaybe (getArg args)
    [Std Pl, Ext Tmpdir]

newTempFilePath :: String -> String -> String -> StdGen -> String
newTempFilePath dir template suffix gen =
    combine dir (base ++ fst (selectN range rcount gen) ++ suffix)
    where range = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
          (rcount, base) =
              (length *** reverse) $ span (== 'X') $ reverse template
