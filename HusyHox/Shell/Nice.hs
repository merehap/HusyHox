module HusyHox.Shell.Nice (main, nice) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness nice

data NiceSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

nice :: Utility NiceSwitch OutputArgs OutputArgs OutputArgs OutputArgs
nice = Utility "nice" argDescs input parse core format output where
    input _ = return $ OA ""

    parse _ = id

    core = id

    format = id

    output (OA text) = putStr text

argDescs :: [Arg (Switch NiceSwitch)]
argDescs =
    [ mkNamedData Nl "adjustment" "N"
          "add integer N to the niceness (default 10)"
    , mkHelp
    , mkVersion
    ]
