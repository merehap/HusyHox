module HusyHox.Shell.Sleep (main, sleep) where

import Control.Concurrent (threadDelay)
import Data.Char (isDigit)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness sleep

data SleepSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data OutputArgs = OA Int

sleep :: Utility SleepSwitch IOArgs OutputArgs OutputArgs OutputArgs
sleep = Utility "sleep" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = OA $ getMilliseconds $ getRequiredStdArg args Unnamed

    core = id

    format = id

    output (OA duration) = threadDelay duration

argDescs :: [Arg (Switch SleepSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    , mkUnnamed Unnamed "DURATION" "DURATION"
    ]

-- Helpers --------------------------------------------------------------------

getMilliseconds :: String -> Int
getMilliseconds text = truncate (1000000 * seconds)
    where lt = last text
          seconds
              | lt `elem` "smhd" = readInterval (init text) * getMultiplier lt
              | isDigit lt = readInterval text
              | otherwise = error ("invalid time interval '" ++ text ++ "'")

getMultiplier :: Char -> Double
getMultiplier = g
    where g 's' = 1
          g 'm' = 60
          g 'h' = 3600
          g 'd' = 86400
          g _   = error "impossible"

readInterval :: String -> Double
readInterval text = read ('0':text)
