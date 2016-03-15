module HusyHox.Shell.Logname (main, logname) where

import Control.Exception (onException)
import System.Posix.User (getLoginName)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness logname

data LognameSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

logname :: Utility LognameSwitch OutputArgs OutputArgs OutputArgs OutputArgs
logname = Utility "logname" argDescs input parse core format output where
    input _ = fmap OA $ onException getLoginName (return "logname: no login name")

    parse _ = id

    core = id

    format = id

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch LognameSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
