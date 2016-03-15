module HusyHox.Shell.Arch (main, arch) where

import System.Posix.Unistd (SystemID(machine), getSystemID)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness arch

data ArchSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA SystemID

data OutputArgs = OA String

arch :: Utility ArchSwitch IOArgs OutputArgs OutputArgs OutputArgs
arch = Utility "arch" argDescs input parse core format output where
    input _ = fmap IA getSystemID

    parse _ (IA sid) = OA $ machine sid

    core = id

    format = id

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch ArchSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
