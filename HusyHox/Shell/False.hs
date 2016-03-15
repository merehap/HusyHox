module HusyHox.Shell.False (main, false) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness false

data FalseSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA

false :: Utility FalseSwitch OutputArgs OutputArgs OutputArgs OutputArgs
false = Utility "false" argDescs input parse core format output where
    input _ = return OA

    parse _ _ = OA

    core _ = OA

    format _ = OA

    output _ = return ()

argDescs :: [Arg (Switch FalseSwitch)]
argDescs = []
