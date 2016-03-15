module HusyHox.Shell.SU (main, su) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness su

data SUSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

su :: Utility SUSwitch OutputArgs OutputArgs OutputArgs OutputArgs
su = Utility "su" argDescs input parse core format output where
    input _ = return $ OA ""

    parse _ = id

    core = id

    format = id

    output (OA text) = putStr text

argDescs :: [Arg (Switch SUSwitch)]
argDescs =
    [ mkNamed Ll "login" "make the shell a login shell"
    , mkNamedData Cl "command" "COMMAND"
          "pass a single COMMAND to the shell with -c"
    , mkNamed Fl "fast" "pass -f to the shell (for csh or tcsh)"
    , mkNamed Ml "preserve-environment" "do not reset environment variables"
    , mkAbbrOnly Pl "same as -m"
    , mkNamedData Sl "shell" "SHELL" "run SHELL if /etc/shells allows it"
    , mkHelp
    , mkVersion
    ]
