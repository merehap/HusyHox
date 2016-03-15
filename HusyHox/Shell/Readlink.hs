module HusyHox.Shell.Readlink (main, readlink) where

import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness readlink

data ReadlinkSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

readlink :: Utility ReadlinkSwitch OutputArgs OutputArgs OutputArgs OutputArgs
readlink = Utility "readlink" argDescs input parse core format output where
    input args = do
        let path = getRequiredArg args (Std Unnamed)
        fileStatus <- getSymbolicLinkStatus path
        fmap OA $ if' (isSymbolicLink fileStatus)
            (readSymbolicLink path)
            (return "")

    parse _ = id

    core = id

    format = id

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch ReadlinkSwitch)]
argDescs =
    [ mkNamed Fl "canonicalize"
          ("canonical by following every symlink in\n" ++
           "every component of the given name recursively;\n" ++
           "all but the last component must exist")
    , mkNamed El "canonicalize-existing"
          ("canonicalize by following every symlink in\n" ++
           "every component of the given name recursively,\n" ++
           "all components must exist")
    , mkNamed Ml "canonicalize-missing"
          ("canonicalized by following every symlink in\n" ++
           "every component of the given name recursively,\n" ++
           "without requirements on components existence")
    , mkNamed Nl "no-newline" "do not output the trailing newline"
    , mkNamed Ql "quiet" ""
    , mkNamed Sl "silent" "suppress most error messages"
    , mkNamed Vl "verbose" "report error messages"
    , mkHelp
    , mkVersion
    , mkRequiredUnnamed Unnamed "FILE" "FILE"
    ]
