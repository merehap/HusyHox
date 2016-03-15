module HusyHox.Shell.Whoami (main, whoami) where

import System.Posix.User (getEffectiveUserName)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness whoami

data WhoamiSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data FormatArgs = FA String

data OutputArgs = OA String

whoami :: Utility WhoamiSwitch FormatArgs FormatArgs FormatArgs OutputArgs
whoami = Utility "whoami" argDescs input parse core format output where
    input _ = fmap FA getEffectiveUserName

    parse _ = id

    core = id

    format (FA text) = OA (text ++ "\n")

    output (OA text) = putStr text

argDescs :: [Arg (Switch WhoamiSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
