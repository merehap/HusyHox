module HusyHox.Shell.Link (main, link) where

import System.Posix.Files (createLink)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness link

data LinkSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

type CoreArgs = OutputArgs

type FormatArgs = OutputArgs

data OutputArgs = OA FilePath FilePath

link :: Utility LinkSwitch IOArgs CoreArgs FormatArgs OutputArgs
link = Utility "link" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ =
        OA (getRequiredStdArg args Unnamed) (getRequiredStdArg args Unnamed2)

    core = id

    format = id

    output (OA source destination) = createLink source destination

argDescs :: [Arg (Switch LinkSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    , mkRequiredUnnamed Unnamed  "FILE1" "FILE1"
    , mkRequiredUnnamed Unnamed2 "FILE2" "FILE2"
    ]
