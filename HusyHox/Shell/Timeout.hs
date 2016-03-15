module HusyHox.Shell.Timeout (main, timeout) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness timeout

data TimeoutSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

timeout :: Utility TimeoutSwitch OutputArgs OutputArgs OutputArgs OutputArgs
timeout = Utility "timeout" argDescs input parse core format output where
    input _ = return $ OA ""

    parse _ = id

    core = id

    format = id

    output (OA text) = putStr text

argDescs :: [Arg (Switch TimeoutSwitch)]
argDescs =
    [ mkNamedData Kl "kill-after" "DURATION"
          ("also send a KILL signal if COMMAND is still running\n" ++
           "this long after the initial signal was sent.")
    , mkNamedData Sl "signal" "SIGNAL"
          ("specify the signal to be sent on timeout\n" ++
           "SIGNAL may be a name like `HUP' or a number.\n" ++
           "See `kill -l` for a list of signals")
    , mkHelp
    , mkVersion
    ]
