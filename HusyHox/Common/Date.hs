module HusyHox.Common.Date where

import System.Time (CalendarTime, getClockTime, toCalendarTime)

getCalendarTime :: IO CalendarTime
getCalendarTime = getClockTime >>= toCalendarTime

defaultDateTimeFormat :: String
defaultDateTimeFormat = "%a %b %e %H:%M:%S %Z %Y"

defaultTimeFormat :: String
defaultTimeFormat = "%H:%M:%S"
