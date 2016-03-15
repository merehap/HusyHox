module HusyHox.File.Mknod (main, mknod) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness mknod

data MknodSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

mknod :: Utility MknodSwitch IOArgs CoreArgs FormatArgs OutputArgs
mknod = Utility "mknod" argDescs input parse core format output where
    input _ = return $ IA ""

    parse _ (IA text) = CA text

    core (CA text) = FA text

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch MknodSwitch)]
argDescs =
    [ mkRequiredUnnamed Unnamed  "NAME"  "NAME"
    , mkRequiredUnnamed Unnamed2 "TYPE"  "TYPE"
    , mkUnnamed         Unnamed3 "MAJOR" "MAJOR"
    , mkUnnamed         Unnamed4 "MINOR" "MINOR"
    , mkNamedData       Z        "mode"  "MODE"
          "set the SELinux security context of NAME to CTX"
    , mkNamedData       Ml       "context" "CTX"
          "set the SELinux security context of NAME to CTX"
    , mkHelp
    , mkVersion
    ]
