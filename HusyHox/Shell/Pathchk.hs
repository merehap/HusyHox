module HusyHox.Shell.Pathchk (main, pathchk) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness pathchk

data PathchkSwitch = Portability deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

pathchk :: Utility PathchkSwitch OutputArgs OutputArgs OutputArgs OutputArgs
pathchk = Utility "pathchk" argDescs input parse core format output where
    input _ = return $ OA ""

    parse _ = id

    core = id

    format = id

    output (OA text) = putStr text

argDescs :: [Arg (Switch PathchkSwitch)]
argDescs =
    [ mkAbbrOnly Pl "check for most POSIX systems"
    , mkAbbrOnly P  "check for empty names and leading \"-\""
    , mkExt Portability "portability"
          "check for all POSIX systems (equivalent to -p -P"
    , mkHelp
    , mkVersion
    ]
