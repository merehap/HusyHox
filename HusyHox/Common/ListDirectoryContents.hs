module HusyHox.Common.ListDirectoryContents (LdcUtility, ldcUtility) where

import Prelude hiding (all)

import Data.List (intercalate, sort)
import System.Directory (getCurrentDirectory, getDirectoryContents)

import HusyHox.Common.Arg
import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility (Utility(..))

type LdcUtility =
    Utility LdcSwitch LdcIOArgs LdcCoreArgs LdcFormatArgs LdcOutputArgs

ldcUtility :: String -> LdcUtility
ldcUtility name =
    Utility name ldcArgDescs ldcInput ldcParse ldcCore ldcFormat ldcOutput

data LdcIOArgs = IA [String]

data LdcCoreArgs = CA IncludeOption Bool [String]

data IncludeOption = Standard | AlmostAll | All

data LdcFormatArgs = FA [String]

data LdcOutputArgs = OA String

data LdcSwitch
    = Author | BlockSize | Color | DeReferenceSymLinkToDir | FileType | Format
    | FullTime | GroupDirectoriesFirst | Hide | IndicatorStyle
    | ShowControlChars | QuotingStyle | Sort | Time | SI | TimeStyle
    deriving (Bounded, Enum, Eq, Ord, Show)

ldcInput :: Args (Switch LdcSwitch) -> IO LdcIOArgs
ldcInput = fmap IA . contents . argsRest

ldcParse :: Args (Switch LdcSwitch) -> LdcIOArgs -> LdcCoreArgs
ldcParse args (IA text) = CA includeOption rev text
    where includeOption = getIncludeOption all almost
          [all, almost, rev] = map (gotArg args . Std) [Al, A, Rl]

ldcCore :: LdcCoreArgs -> LdcFormatArgs
ldcCore (CA includeOption rev names) = FA $
    if' rev reverse id (getIncludedNames includeOption names)

ldcFormat :: LdcFormatArgs -> LdcOutputArgs
ldcFormat (FA names) = OA $ intercalate "  " (sort names) ++ "\n"

ldcOutput :: LdcOutputArgs -> IO ()
ldcOutput (OA text) = putStr text

ldcArgDescs :: [Arg (Switch LdcSwitch)]
ldcArgDescs =
    [ mkNamed Al "all" "Do not ignore ldcle names that start with '.'."
    , mkNamed A  "almost-all" "Ignore only ldcles named '.' and '..'"
    , mkExt Author "author" "with -l, print the author of each ldcle"
    , mkNamed Bl "escape" "print C-style escapes for nongraphic characters"
    , mkNamed B  "ignore-backups" "Ignore ldcles that end with '~'."
    , mkAbbrOnly Cl ("with -lt: sort by, and show, ctime (time of last\n" ++
          "  modildccation of file status information)\n" ++
          "  with -l: show ctime and sort by name\n" ++
          "  otherwise: sort by ctime")
    , mkAbbrOnly C "list entries by columns"
    , mkNamed Dl "directory" "List only the names of directories."
    , mkNamed D "dired" "generated output designed for Emacs' dired mode"
    , mkAbbrOnly Fl "do not sort, enable -aU, disable -ls --color"
    , mkNamed F "classify" "append indicator (one of */=>@!) to entries"
    , mkExt FileType "ldcle-type" "likewise, except do not append `*'"
    , mkExtData Format "format" "WORD"
          ("across -x, commas -m, horizontal -x, long -l" ++
           "  single-column -1, verbose -l, vertical -C")
    , mkExt FullTime "full-time" "like -l --time-style=full-iso"
    , mkAbbrOnly Gl "like -l, but do not list owner"
    , mkExt GroupDirectoriesFirst "group-directories-ldcrst"
          ("group directories before ldcles." ++
           "  augment with a --sort option, but any" ++
           "  use of --sort=none (-U) disables grouping")
    , mkNamed G "no-group" "in a long listing, don't print group names"
    , mkNamed Hl "human-readable"
          ("with -l, print sizes in human readable format" ++
           "  (e.g., 1K 234M 2G)")
    , mkExt SI "si" "likewise, but use powers of 1000 not 1024"
    , mkNamed H  "dereference-command-line"
          ("Show information about the ldcle a symbolic link points to\n" ++
           "rather than about the symbolic link itself.")
    , mkExt DeReferenceSymLinkToDir
          "dereference-command-line-symlink-to-dir"
          "Only dereference symbolic links to directories."
    , mkExtData Hide "hide" "pattern"
          "Ignore ldcles which match 'pattern' unless -all is specified."
    , mkExtData IndicatorStyle "indicator-style" "WORD"
          ("append indicator with  style WORD to entry names:\n" ++
           "  none (default), slash (-p),\n" ++
           "  ldcle-type (--file-type), classify (-F)")
    , mkNamed Il "inode" "print the index number of each ldcle"
    , mkNamedData I "ignore" "pattern" "Ignore ldcles which match 'pattern'."
    , mkAbbrOnly Kl "like --block-size=1K"
    , mkAbbrOnly Ll "use a long listing format"
    , mkNamed L "dereference" "Show symbolic link name and data of target."
    , mkAbbrOnly Ml "ldcll width with a comma separated list of entries"
    , mkNamed Nl "numeric-uid-gid"
          "like -l, but list numeric user and group IDs"
    , mkNamed N "literal"
          ("print raw entry names (don't treat e.g. control\n" ++
           "  characters specially)")
    , mkNamed Rl "reverse" "reverse order while sorting"
    , mkNamed R "recursive"
          "List the contents of all directories recursively."
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

contents :: [String] -> IO [String]
contents [] = getCurrentDirectory >>= getDirectoryContents
contents xs = fmap concat $ mapM getDirectoryContents xs

getIncludeOption :: Bool -> Bool -> IncludeOption
getIncludeOption = g
    where g True _    = All
          g _    True = AlmostAll
          g _    _    = Standard

getIncludedNames :: IncludeOption -> [String] -> [String]
getIncludedNames option = filter (f option)
    where f All       _ = True
          f AlmostAll x = x /= "." && x /= ".."
          f Standard  x = take 1 x /= "."
