module HusyHox.File.Chown (main, chown) where

import System.Posix.Files (setOwnerAndGroup)
import System.Posix.Types (UserID)
import System.Posix.User

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness chown

data ChownSwitch
    = Dereference | From | NoPreserveRoot | PreserveRoot | Reference
    deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA UserEntry

data CoreArgs = CA UserID String

data FormatArgs = FA UserID String

data OutputArgs = OA UserID String

chown :: Utility ChownSwitch IOArgs CoreArgs FormatArgs OutputArgs
chown = Utility "chown" argDescs input parse core format output where
    input = fmap IA . getUserEntryForName . flip getRequiredArg (Std Unnamed)

    parse args (IA userEntry) = CA uid path
        where uid  = userID userEntry
              path = getRequiredArg args $ Std Unnamed2

    core (CA uid path) = FA uid path

    format (FA uid path) = OA uid path

    output (OA uid path) = setOwnerAndGroup path uid (-1)

argDescs :: [Arg (Switch ChownSwitch)]
argDescs =
    [ mkUnnamed Unnamed  "USER" "USER"
    , mkUnnamed Unnamed2 "FILE" "FILE"
    , mkNamed Cl "changes" "like verbose but report only when a change is made"
    , mkExt Dereference "dereference"
          ("affect the referent of each symbolic link (this is" ++
           "the default), rather than the symbolic link itself")
    , mkNamed Hl "no-dereference"
          ("affect each symbolic link instead of any referenced\n" ++
           "file (useful only on systems that can change the\n" ++
           "ownership of a symlink)")
    , mkExtData From "from" "CURRENT_OWNER:CURRENT_GROUP"
          ("change the owner and/or group of each file only if\n" ++
           "its current owner and/or group match those specified\n" ++
           "here. Either may be omitted, in which case a match\n" ++
           "is not required for the omitted attribute")
    , mkExt NoPreserveRoot "no-preserve-root"
          "do not treat `/' specially (the default)"
    , mkExt PreserveRoot "preserve-root" "fail to operated recursively on `/'"
    , mkNamed Fl "silent" "suppress most error messages"
    , mkExtData Reference "reference" "RFILE"
          ("use RFILE's owner and group rather than\n" ++
           "specifying OWNER:GROUP values")
    , mkNamed R "recursive" "operated on files and directories recursively"
    , mkNamed Vl "verbose" "output a diagnostic for every file processed"
    , mkHelp
    , mkVersion
    ]
