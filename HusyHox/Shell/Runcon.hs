module HusyHox.Shell.Runcon (main, runcon) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness runcon

data RunconSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

runcon :: Utility RunconSwitch OutputArgs OutputArgs OutputArgs OutputArgs
runcon = Utility "runcon" argDescs input parse core format output where
    input _ = return $ OA ""

    parse _ = id

    core = id

    format = id

    output (OA text) = putStr text

argDescs :: [Arg (Switch RunconSwitch)]
argDescs =
    [ mkNamed Cl "compute"
          "compute process transition context before modifying"
    , mkNamedData Tl "type" "TYPE" "type (for same role as parent)"
    , mkNamedData Ul "user" "USER" "user identity"
    , mkNamedData Rl "role" "ROLE" "role"
    , mkNamedData Ll "range" "RANGE" "levelrange"
    , mkHelp
    , mkVersion
    ]
