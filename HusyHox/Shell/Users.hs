module HusyHox.Shell.Users (main, users) where

import HusyHox.Common.PseudoTerminal (PtsInfo(ownerName), getPtsInfos)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness users

data UsersSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [PtsInfo]

data FormatArgs = FA [String]

data OutputArgs = OA String

users :: Utility UsersSwitch IOArgs FormatArgs FormatArgs OutputArgs
users = Utility "users" argDescs input parse core format output where
    input _ = fmap IA getPtsInfos

    parse _ (IA ptsInfos) = FA (map ownerName ptsInfos)

    core = id

    format (FA owners) = (OA . unwords) owners

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch UsersSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
