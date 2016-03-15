module HusyHox.Shell.Pwd (main, pwd) where

import System.Directory (getCurrentDirectory)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness pwd

data PwdSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

pwd :: Utility PwdSwitch OutputArgs OutputArgs OutputArgs OutputArgs
pwd = Utility "pwd" argDescs input parse core format output where
    input _ = fmap OA getCurrentDirectory

    parse _ = id

    core = id

    format = id

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch PwdSwitch)]
argDescs =
    [ mkNamed L "logical"
          "use PWD from environment, even if it contains symlinks"
    , mkNamed P "physical" "avoid all symlinks"
    ]
