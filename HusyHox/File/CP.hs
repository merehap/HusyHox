module HusyHox.File.CP (main, cp) where

import System.Directory (copyFile)

import HusyHox.Common.File (manyToOnePath)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness cp

data CPSwitch
    = Backup | CopyContents | Preserve | NoPreserve | Parents
    | Reflink | RemoveDestination | Sparse | StripTrailingSlashes
    deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data OutputArgs = OA [String] String

cp :: Utility CPSwitch IOArgs OutputArgs OutputArgs OutputArgs
cp = Utility "cp" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = (uncurry OA . manyToOnePath . argsRest) args

    core = id

    format = id

    output (OA inputPaths outputPath) =
        mapM_ (`copyFile` outputPath) inputPaths

argDescs :: [Arg (Switch CPSwitch)]
argDescs =
    [ mkNamed Al "archive" "same as -dR --preserve=all"
    , mkExtData Backup "backup" "CONTROL"
          "make a backup of each existing destination file"
    , mkAbbrOnly Bl "like --backup but does not accept an argument"
    , mkExt CopyContents "copy-contents"
          "copy contents of special files when recursive"
    , mkAbbrOnly Dl "same as --no-dereference --preserve=links"
    , mkNamed Fl "force"
          ("if an existing destination file cannot be\n" ++
           "opened, remove it and try again (redundant if\n" ++
           "the -n option is used")
    , mkNamed Il "interactive"
          ("prompt before overwrite (overrides a previous -n\n" ++
           "option")
    , mkAbbrOnly H "follow command-line symbolic links in SOURCE"
    , mkNamed Ll "link" "link files instead of copying"
    , mkNamed L "dereference" "always follow symbolic links in SOURCE"
    , mkNamed Nl "no-clobber"
          ("do not overwrite an existing file (overrides\n" ++
           "a previous -i option)")
    , mkNamed P "no-dereference" "never follow symbolic links in SOURCE"
    , mkAbbrOnly Pl "same as --preserve=mode,ownership,timestamps"
    , mkExtData Preserve "preserve" "ATTR_LIST"
          ("preserve the specified attributes (default:\n" ++
           "mode,ownership,timestamps), if possible\n" ++
           "additional attributes: context, links, xattr,\n" ++
           "all")
    , mkExtData NoPreserve "no-preserve" "ATTR_LIST"
          "don't preserve the specified attributes"
    , mkNamed Rl "recursive" "copy directories recursively"
    , mkExtData Reflink "reflink" "WHEN" "control clone/CoW copies. See below"
    , mkExt RemoveDestination "remove-destination"
          ("remove each existing destination file before\n" ++
           "attempting to open it (contrast with --force)")
    , mkExtData Sparse "sparse" "WHEN"
          "control creation of sparse files. See below"
    , mkExt StripTrailingSlashes "strip-trailing-slashes"
          ("remove any trailing slashes from each SOURCE\n" ++
           "argument")
    , mkNamed Sl "symbolic-link" "make symbolic links instead of copying"
    , mkNamedData S "suffix" "SUFFIX" "override the usual backup suffix"
    , mkNamedData Tl "target-directory" "DIRECTORY"
          "copy all SOURCE arguments into DIRECTORY"
    , mkNamed T "no-target-directory" "treat DEST as a normal file"
    , mkNamed Ul "update"
          ("copy only when the SOURCE file is newer\n" ++
           "than the destination file or when the\n" ++
           "destination file is missing")
    , mkNamed Vl "verbose" "explain what is being done"
    , mkNamed Xl "one-file-system" "stay on this file system"
    , mkHelp
    , mkVersion
    ]
