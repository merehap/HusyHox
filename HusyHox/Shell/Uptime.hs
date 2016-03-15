module HusyHox.Shell.Uptime (main, uptime) where

import Control.Monad (liftM3)
import Data.List (intercalate)
import System.Locale (defaultTimeLocale)
import System.Time (CalendarTime, formatCalendarTime)
import Text.Printf (printf)

import HusyHox.Common.Date (defaultTimeFormat, getCalendarTime)
import HusyHox.Common.PseudoTerminal (getPtsInfos)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness uptime

data UptimeSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA Int CalendarTime String

data CoreArgs = CA Int CalendarTime Double

data FormatArgs = FA Int CalendarTime Double

data OutputArgs = OA String

uptime :: Utility UptimeSwitch IOArgs CoreArgs FormatArgs OutputArgs
uptime = Utility "uptime" argDescs input parse core format output where
    input _ = liftM3 IA
         (fmap length getPtsInfos) getCalendarTime (readFile "/proc/uptime")

    parse _ (IA userCount calendarTime uptimeText) =
        (CA userCount calendarTime . read . head . words) uptimeText

    core (CA userCount calendarTime uptimeSeconds) =
        FA userCount calendarTime uptimeSeconds

    format (FA userCount calendarTime seconds) = OA text
        where text = printf " %s up %s,  %s,  %s"
                  calendarTimeText uptimeText userText loadAverageText
              day  = show (floor (seconds / (24 * 60 * 60)) :: Int)
              hour = show (floor (seconds / (60 * 60)) `mod` 24 :: Int)
              minute = show (floor (seconds / 60) `mod` 60 :: Int)
              calendarTimeText = formatCalendarTime
                  defaultTimeLocale defaultTimeFormat calendarTime
              uptimeText = case  (day, hour) of
                  ("0", "0") -> minute ++  " min"
                  ("0",   _) -> concat
                      [padLeft 2 ' ' hour, ":", padLeft 2 '0' minute]
                  (_  ,   _) -> intercalate ":" $ map (uncurry (padLeft 2))
                      [(' ', day), ('0', hour), ('0', minute)]
              userText = show userCount ++ " users"
              padLeft i c xs = replicate (max 0 (i - length xs)) c ++ xs
              loadAverageText = "load average: 0.00, 0.00, 0.00"

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch UptimeSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
