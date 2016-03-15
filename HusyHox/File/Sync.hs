module HusyHox.File.Sync (main, sync) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness sync

data SyncSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA

sync :: Utility SyncSwitch OutputArgs OutputArgs OutputArgs OutputArgs
sync = Utility "sync" argDescs input parse core format output where
    input _ = return OA

    parse _ _ = OA

    core _ = OA

    format _ = OA

    output _ = putStr ""

argDescs :: [Arg (Switch SyncSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
