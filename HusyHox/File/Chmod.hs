module HusyHox.File.Chmod (main, chmod) where

import Data.Bits ((.|.), (.&.))
import Data.Maybe (fromMaybe)
import System.Posix.Files ( FileStatus, fileMode, getFileStatus, setFileMode
                          , ownerReadMode, ownerWriteMode, ownerExecuteMode)
import System.Posix.Types (FileMode)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness chmod

data ChmodSwitch = NoPreserveRoot | PreserveRoot | Reference
    deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA FilePath FileStatus

data OutputArgs = OA FileMode String

chmod :: Utility ChmodSwitch IOArgs OutputArgs OutputArgs OutputArgs
chmod = Utility "chmod" argDescs input parse core format output where
    input args = do
        status <- getFileStatus path
        return $ IA path status
        where path = getRequiredArg args $ Std Unnamed2

    parse args (IA path status) = OA newMode path
        where perms = getRequiredArg args $ Std Unnamed
              (shouldAdd, modeChanges) = fromMaybe
                  (error ("Permissions not in correct format: " ++ perms))
                  (parseFileMode perms)
              currentMode = fileMode status
              newMode     = modifyFileMode shouldAdd modeChanges currentMode

    core = id

    format = id

    output (OA mode path) = setFileMode path mode

argDescs :: [Arg (Switch ChmodSwitch)]
argDescs =
    [ mkRequiredUnnamed Unnamed  "PERMISSIONS" "PERMISSIONS"
    , mkRequiredUnnamed Unnamed2 "FILE" "FILE"
    , mkNamed Cl "changes" "like verbose but report only when a change is made"
    , mkExt NoPreserveRoot "no-preserve-root"
          "do not treat `/' specially (the default)"
    , mkExt PreserveRoot "preserve-root" "fail to operate recursively on `/'"
    , mkNamed Fl "silent" "suppress most error messages"
    , mkNamed Vl "verbose" "output a diagnostic for every file processed"
    , mkExtData Reference "reference" "RFILE"
          "use RFILE's mode instead of MODE values"
    , mkNamed Rl "recursive" "changed files and directories recursively"
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

modifyFileMode :: Bool -> FileMode -> FileMode -> FileMode
modifyFileMode shouldAdd newPerms oldPerms = newPerms `modifier` oldPerms
    where modifier = if' shouldAdd (.|.) (.&.)

parseFileMode :: String -> Maybe (Bool, FileMode)
parseFileMode [m,p] | isPermission m p = fmap ((,) shouldAdd) permission
    where shouldAdd   = m == '+'
          permission = Just $ case p of
              'r' -> ownerReadMode
              'w' -> ownerWriteMode
              _   -> ownerExecuteMode
parseFileMode _ = Nothing

isPermission :: Char -> Char -> Bool
isPermission m p = elem m "-+" && elem p "rwx"
