module HusyHox.File.Mkfifo (main, mkfifo) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness mkfifo

data MkfifoSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

mkfifo :: Utility MkfifoSwitch IOArgs CoreArgs FormatArgs OutputArgs
mkfifo = Utility "mkfifo" argDescs input parse core format output where
    input _ = return $ IA ""

    parse _ (IA text) = CA text

    core (CA text) = FA text

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch MkfifoSwitch)]
argDescs =
    [ mkNamedData Ml "mode" "MODE"
          "set file permission bits to MODE, not a=rw - umask"
    , mkNamedData Z "context" "CTX"
          "set the SELinux security context of each NAME to CTX"
    , mkHelp
    , mkVersion
    ]
