module HusyHox.File.Chgrp (main, chgrp) where

import System.Posix.Files (setOwnerAndGroup)
import System.Posix.Types (GroupID)
import System.Posix.User (GroupEntry, getGroupEntryForName, groupID)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness chgrp

data ChgrpSwitch = Dereference | NoPreserveRoot | PreserveRoot | Reference
    deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA GroupID String

chgrp :: Utility ChgrpSwitch GroupEntry OutputArgs OutputArgs OutputArgs
chgrp = Utility "chgrp" argDescs input parse noCore noFormat output where
    input args = do
        let newGroupName = getRequiredArg args $ Std Unnamed
        getGroupEntryForName newGroupName

    parse args newGroupEntry = OA newGroupID path
        where path = getRequiredArg args $ Std Unnamed2
              newGroupID = groupID newGroupEntry

    output (OA gid path) = setOwnerAndGroup path (-1) gid

argDescs :: [Arg (Switch ChgrpSwitch)]
argDescs =
    [ mkRequiredUnnamed Unnamed  "GROUP" "GROUP"
    , mkRequiredUnnamed Unnamed2 "FILE" "FILE"
    , mkNamed Cl "changes" "like verbose but report only when a change is made"
    , mkExt Dereference "dereference"
          ("affect the referent of each symbolic link (this is\n" ++
           "the default), rather than the symbolic link itself")
    , mkNamed Hl "nodereference"
          ("affect each symbolic link instead of any referenced\n" ++
           "file (useful only on systems that can change the\n" ++
           "ownership of a symlink")
    , mkExt NoPreserveRoot "no-preserve-root"
           "do not treat `/' specially (the default)"
    , mkExt PreserveRoot "preserve-root" "fail to operate recursively on `/'"
    , mkNamed Fl "silent" "suppress most error messages"
    , mkExtData Reference "reference" "RFILE"
          "use RFILE's group rather than specifying a GROUP value"
    , mkNamed R  "recursive" "operate on files and directories recursively"
    , mkNamed Vl "verbose" "output a diagnostic for every file processed"
    , mkAbbrOnly H "if a command line argument is symbolic link"
    , mkAbbrOnly L "traverse every symbolic link to a directory encountered"
    , mkAbbrOnly P "do not traverse any symbolic links (default)"
    , mkHelp
    , mkVersion
    ]
