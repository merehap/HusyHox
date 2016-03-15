module HusyHox.File.Install (main, install) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness install

data InstallSwitch = Backup | StripProgram | PreserveContext
    deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

install :: Utility InstallSwitch IOArgs CoreArgs FormatArgs OutputArgs
install = Utility "install" argDescs input parse core format output where
    input _ = return $ IA ""

    parse _ (IA text) = CA text

    core (CA text) = FA text

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch InstallSwitch)]
argDescs =
    [ mkUnnamed Unnamed "SOURCE" "SOURCE"
    , mkUnnamed Unnamed "DESTINATION" "DESTINATION"
    , mkExtData Backup "backup" "CONTROL"
          "make a backup of each existing destination file"
    , mkAbbrOnly Bl "like --backup but does not accept an argument"
    , mkAbbrOnly Cl "(ignored)"
    , mkNamed C "compare"
          ("compare each pair of source and destination files, and\n" ++
           "in some cases, do not modify the destination at all")
    , mkNamed Dl "directory"
          ("treat all arguments as directory names; create all\n" ++
           "components of the specified directories")
    , mkAbbrOnly D
          ("create all leading components of DEST except the last,\n" ++
           "then copy SOURCE to DEST")
    , mkNamedData Gl "group" "GROUP"
          "set group ownership, instead of process' current group"
    , mkNamedData Ml "mode" "MODE"
          "set permission mode (as in chmod), instead of rwxr-xr-x"
    , mkNamedData Ol "owner" "OWNER" "set ownership (super-user only)"
    , mkNamed Pl "preserve-timestamps"
          ("apply access/modification times of SOURCE files\n" ++
           "to corresponding destination files")
    , mkNamed Sl "strip" "strip symbol tables"
    , mkExtData StripProgram "strip-program" "PROGRAM"
          "program used to strip binaries"
    , mkNamedData S "suffix" "SUFFIX" "override the usual backup suffix"
    , mkNamedData Tl "target-directory" "DIRECTORY"
          "copy all SOURCE arguments into DIRECTORY"
    , mkNamed T "no-target-directory" "treat DEST as a normal file"
    , mkNamed Vl "verbose" "print the name of each directory as it is created"
    , mkExt PreserveContext "preserve-context"
          "preserve SELinux security context"
    , mkNamedData Zl "context" "CONTEXT"
          "set SELinux security context of files and directories"
    , mkHelp
    , mkVersion
    ]
