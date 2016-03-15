module HusyHox.Shell.Unlink (main, unlink) where

import System.Posix.Files (removeLink)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness unlink

data UnlinkSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data OutputArgs = OA FilePath

unlink :: Utility UnlinkSwitch IOArgs OutputArgs OutputArgs OutputArgs
unlink = Utility "unlink" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = OA (getRequiredStdArg args Unnamed)

    core = id

    format = id

    output (OA path) = removeLink path

argDescs :: [Arg (Switch UnlinkSwitch)]
argDescs =
    [ mkHelp 
    , mkVersion
    , mkRequiredUnnamed Unnamed "FILE" "FILE"
    ]
