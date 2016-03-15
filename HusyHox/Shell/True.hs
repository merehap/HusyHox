module HusyHox.Shell.True (main, true) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness true

data TrueSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA

true :: Utility TrueSwitch OutputArgs OutputArgs OutputArgs OutputArgs
true = Utility "true" argDescs input parse core format output where
    input _ = return OA

    parse _ _ = OA

    core _ = OA

    format _ = OA

    output _ = return ()

argDescs :: [Arg (Switch TrueSwitch)]
argDescs = []
