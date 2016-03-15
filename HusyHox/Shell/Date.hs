module HusyHox.Shell.Date (main, date) where

import Data.Maybe (fromMaybe)
import System.Locale (defaultTimeLocale)
import System.Time (CalendarTime, formatCalendarTime)

import HusyHox.Common.Date (defaultDateTimeFormat, getCalendarTime)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness date

data DateSwitch = Rfc3339 deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA CalendarTime

data CoreArgs = CA String CalendarTime

data FormatArgs = FA String

data OutputArgs = OA String

date :: Utility DateSwitch IOArgs CoreArgs FormatArgs OutputArgs
date = Utility "date" argDescs input parse core format output where
    input _ = fmap IA getCalendarTime

    parse args (IA calendarTime) = CA dateFormat calendarTime
        where maybeFormat = getStdArg args Unnamed
              dateFormat = fromMaybe defaultDateTimeFormat maybeFormat

    core (CA dateFormat calendarTime) = FA $
        formatCalendarTime defaultTimeLocale dateFormat calendarTime

    format (FA text) = OA text

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch DateSwitch)]
argDescs =
    [ mkNamedData Dl "date" "STRING"
          "display time described by STRING, not `now'"
    , mkNamedData Fl "file" "DATEFILE"
          "like --date once for each line of DATEFILE"
    , mkNamedData Rl "reference" "FILE"
          "display the last modification time of FILE"
    , mkNamed R "rfc-2822" "output date and time in RFC 2822 format."
    , mkExtData Rfc3339 "rfc-3339" "TIMESPEC"
          ("output date and time in RFC 2822 format." ++
           "Example: Mon, 07 Aug 2006 12:34:56 -0600")
    , mkNamedData Sl "set" "STRING" "set time described by STRING"
    , mkNamed Ul "utc" "print or set Coordinated Universal Time"
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "FORMAT" "FORMAT"
    ]
