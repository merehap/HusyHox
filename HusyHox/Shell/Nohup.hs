module HusyHox.Shell.Nohup (main, nohup) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness nohup

data NohupSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

nohup :: Utility NohupSwitch OutputArgs OutputArgs OutputArgs OutputArgs
nohup = Utility "nohup" argDescs input parse core format output where
    input _ = return $ OA ""

    parse _ = id

    core = id

    format = id

    output (OA text) = putStr text

argDescs :: [Arg (Switch NohupSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
