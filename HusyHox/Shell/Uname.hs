module HusyHox.Shell.Uname (main, uname) where

import Control.Arrow (first)
import Data.Maybe (catMaybes)
import System.Info (arch)
import System.Posix.Unistd (SystemID(..), getSystemID)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness uname

data UnameSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

type MS = Maybe String

data IOArgs = IA SystemID

data FormatArgs = FA MS MS MS MS MS MS MS MS

data OutputArgs = OA String

uname :: Utility UnameSwitch IOArgs FormatArgs FormatArgs OutputArgs
uname = Utility "uname" argDescs input parse core format output where
    input _ = fmap IA getSystemID

    parse args (IA sid) = FA ts n r v m p i o
        where a = gotStdArg args Al
              options = map (first ((|| a) . gotStdArg args)) possibleOptions
              t@[s, n, r, v, m, p, i, o] = map (uncurry toMaybe) options
              ts = if' (null $ catMaybes t) (Just $ systemName sid) s
              possibleOptions =
                  [ (Sl, systemName sid)
                  , (Nl, nodeName sid  )
                  , (Rl, release sid   )
                  , (Vl, version sid   )
                  , (Ml, machine sid   )
                  , (Pl, machine sid   )
                  , (Il, arch          )
                  , (Ol, "GNU/Linux"   )
                  ]

    core = id

    format (FA s n r v m p i o) = OA $
        unwords $ catMaybes [s, n, r, v, m, p, i, o]

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch UnameSwitch)]
argDescs =
    [ mkNamed Al "all"
          ("print all information, in the following order,\n" ++
           "except omit -p and -i if unknown")
    , mkNamed Sl "kernel-name" "print the kernel name"
    , mkNamed Nl "nodename"    "print the network node hostname"
    , mkNamed Rl "kernel-release" "print the kernel release"
    , mkNamed Vl "kernel-version" "print the kernel version"
    , mkNamed Ml "machine" "print the machine hardware name"
    , mkNamed Pl "processor" "print the processor type of \"unknown\""
    , mkNamed Il "hardware-platform"
          "print the hardware platform or \"unknown\""
    , mkNamed Ol "operating-system" "print the operating system"
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

toMaybe :: Bool -> a -> Maybe a
toMaybe b = if' b Just (const Nothing)
