{-# LANGUAGE ForeignFunctionInterface #-}

module HusyHox.Shell.Hostid (main, hostid) where

import Foreign.C.Types

import HusyHox.Common.NumberBase (showHexadecimal)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness hostid

data HostidSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data FormatArgs = FA CLong

data OutputArgs = OA String

hostid :: Utility HostidSwitch FormatArgs FormatArgs FormatArgs OutputArgs
hostid = Utility "hostid" argDescs input parse core format output where
    input _ = fmap FA c_gethostid

    parse _ = id

    core = id

    format (FA hid) = OA $ showHexadecimal hid

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch HostidSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]

foreign import ccall safe "gethostid"
    c_gethostid :: IO CLong
