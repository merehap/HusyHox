module HusyHox.File.Touch (main, touch) where

import System.Directory (doesFileExist)
import System.Posix.Files (touchFile)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness touch

data TouchSwitch = Time deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [(FilePath, Bool)]

data OutputArgs = OA Bool [(FilePath, Bool)]

touch :: Utility TouchSwitch IOArgs OutputArgs OutputArgs OutputArgs
touch = Utility "touch" argDescs input parse core format output where
    input args = do
        let paths = argsRest args
        exists <- mapM doesFileExist paths
        return $ IA (zip paths exists)

    parse args (IA fileInfos) = OA shouldCreate fileInfos
        where shouldCreate = not (gotArg args $ Std Cl)

    core = id

    format = id

    output (OA shouldCreate fileInfos) =
        mapM_ (\(p, e) -> case (shouldCreate, not e) of
            (True, False) -> writeFile p ""
            (_   ,  True) -> touchFile p
            (_   ,     _) -> return ()) fileInfos

argDescs :: [Arg (Switch TouchSwitch)]
argDescs =
    [ mkAbbrOnly Al "change only the access time"
    , mkNamed Cl "no-create" "do not create any new files"
    , mkNamedData Dl "date" "STRING"
          "parse STRING and use it instead of current time"
    , mkAbbrOnly Fl "(ignored)"
    , mkNamed Hl "no-dereference"
          ("affect each symbolic link instead of any referenced\n" ++
           "file (useful only on systems that can change the\n" ++
           "timestamps of a symlink")
    , mkAbbrOnly Ml "change only the modification time"
    , mkNamedData Rl "reference" "FILE"
          "use this file's times instead of current time"
    , mkAbbrOnlyData Tl "STAMP"
          "use [[CC]YY]MMDDhhmm[.ss] instead of current time"
    , mkExtData Time "time" "WORD"
          ("change the specified time:" ++
           "  WORD is access, atime, or use: equivalent to -a\n" ++
           "  WORD is modify or mtime: equivalent to -m")
    , mkHelp
    , mkVersion
    ]
