module HusyHox.Text.PR (main, pr) where

import Data.List.Split (splitEvery)
import System.Time (CalendarTime, getClockTime, toCalendarTime)

import HusyHox.Common.Utility
import HusyHox.Common.Extra (readInput)

main :: IO ()
main = runUtilityHarness pr

data PRSwitch = Pages | Columns deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA CalendarTime String

data CoreArgs = CA CalendarTime String

data FormatArgs = FA [String]

data OutputArgs = OA String

pr :: Utility PRSwitch IOArgs CoreArgs FormatArgs OutputArgs
pr = Utility "pr" argDescs input parse core format output where
    input args = do
        clockTime    <- getClockTime
        calendarTime <- toCalendarTime clockTime
        text         <- (readInput . getArg args . Std) Unnamed
        return $ IA calendarTime text

    parse _ (IA date text) = CA date text

    core (CA date text) = FA $ map formatPage texts
        where texts = (splitEvery 56 . lines) text
              formatPage :: [String] -> String
              formatPage xs = unlines (
                ["", "", show date, "", ""] ++ xs ++ replicate 5 "")

    format (FA lns) = OA $ concat lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch PRSwitch)]
argDescs =
    [ mkExtData   Pages   "pages"   "FIRST_PAGE[:LAST_PAGE]"
          "begin [stop] printing with page FIRST_PAGE[:LAST_PAGE]"
    , mkExtData   Columns "columns" "COLUMN"
          ("output COLUMN columns and print columns down, unless -a is used."
          ++ "Balance number of lines in the columns on each page")
    , mkNamed     Al "across"
          ("print columns across rather than down," ++
           "used together with --columns")
    , mkNamed     Cl "show-control-chars"
          "use hat notations (^G) and octal backslash notation"
    , mkNamed     Dl "double-space" "double space the output"
    , mkNamedData D  "date-format" "FORMAT" "use FORMAT for the header date"
    , mkNamedData El "expand-tabs" "[CHAR[WIDTH]]"
          "expand input CHARs (TABs) to tab WIDTH (8)"
    , mkNamed     Fl "form-feed"
          ("use form feeds instead of newlines to separate pages" ++
           "(by a 5-line header)")
    , mkAbbrOnly  F  "like -f, but a 3-line header rather than 5-line"
    , mkNamedData Hl "header" "HEADER"
          ("use a centered HEADER instead of filename in page header, " ++
           "-h \"\" prints a blank line, don't use -h\"\"")
    , mkNamedData Il "output-tabs" "[CHAR[WIDTH]]"
          "replace spaces with CHARs (TABs) to tab WIDTH (8)"
    , mkNamed     J  "join-lines"
          ("merge full lines, turns off -W line truncation, " ++
           "no column alignment, --sep-string[=STRING] sets separators")
    , mkNamedData Ll "length" "PAGE_LENGTH"
          ("set the page length to PAGE_LENGTH (66) lines" ++
           "(default number of lines of text 56, and with -F 63")
    , mkNamed     Ml "merge"
          ("print all files in parallel, one in each column," ++
           "truncate lines, but join lines of full length with -J")
    , mkNamedData Nl "number-lines" "[SEP[DIGITS]]"
          ("number lines, use DIGITS (5) digits, then SEP (TAB)," ++
           "default counting starts with 1st line of input file")
    , mkNamedData N  "first-line-number" "NUMBER"
          ("start counting with NUMBER at 1st line of first" ++
           "page printed (see +FIRST_PAGE)")
    , mkNamedData Ol "indent" "MARGIN"
          ("offset each line with MARGIN (zero) spaces, do not" ++
           "affect -w or -W, MARGIN will be added to PAGE_WIDTH")
    , mkNamed     Rl "no-file-warnings"
          "omit warning when a file cannot be opened"
    , mkNamedData Sl "sep-string" "STRING"
          ("separate columns by STRING," ++
           "without -S: Default separator <TAB> with -J and <space>" ++
           "otherwise (same as -S\" \"), no effect on column options")
    , mkNamed     Tl "omit-header"     "omit page headers and trailers"
    , mkNamed     T  "omit-pagination"
          ("omit page headers and trailers, eliminate any pagination" ++
           "by form feeds set in input files")
    , mkNamed     Vl "show-nonprinting" "use octal backslash notation"
    , mkNamedData Wl "page-width" "PAGE-WIDTH"
          ("set page width to PAGE_WIDTH (72) characters always," ++
           "truncate lines, except -J option is set, no interference" ++
           "with -S or -s")
    , mkHelp
    , mkVersion
    ]
