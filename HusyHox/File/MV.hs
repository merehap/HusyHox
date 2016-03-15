module HusyHox.File.MV (main, mv) where

import System.Directory (copyFile, removeFile)

import HusyHox.Common.File (manyToOnePath)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness mv

data MVSwitch = Backup | StripTrailingSlashes deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data OutputArgs = OA [String] String

mv :: Utility MVSwitch IOArgs OutputArgs OutputArgs OutputArgs
mv = Utility "mv" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = (uncurry OA . manyToOnePath . argsRest) args

    core = id

    format = id

    output (OA inputPaths outputPath) = do
        mapM_ (`copyFile` outputPath) inputPaths
        mapM_ removeFile inputPaths

argDescs :: [Arg (Switch MVSwitch)]
argDescs =
    [ mkUnnamed Unnamed  "SOURCE"      "SOURCE"
    , mkUnnamed Unnamed2 "DESTINATION" "DESTINATION"
    , mkExtData Backup "backup" "CONTROL"
          "make a backup of each existing destination file"
    , mkAbbrOnly Bl "like --backup but does not accept an argument"
    , mkNamed Fl "force" "do not prompt before overwriting"
    , mkNamed Il "interactive" "prompt before overwrite"
    , mkNamed Nl "no-clobber" "do not overwrite an existing file"
    , mkExt StripTrailingSlashes "strip-trailing-slashes"
          ("remove  any trailing slashes from each SOURCE" ++
           "argument")
    , mkNamedData S "suffix" "SUFFIX" "override the usual backup suffix"
    , mkNamed Tl "target-directory-directory" "treat DEST as a normal file"
    , mkNamed Ul "update"
          ("move only when the SOURCE file is newer\n" ++
           "  than the destination file or when the\n" ++
           "  destination file is missing\n")
    , mkNamed Vl "verbose" "explain what is being done"
    , mkHelp
    , mkVersion
    ]
