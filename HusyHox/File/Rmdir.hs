module HusyHox.File.Rmdir (main, rmdir) where

import Control.Monad (forM_, unless)
import System.Directory (getDirectoryContents, removeDirectory)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness rmdir

data RmdirSwitch = IgnoreFailOnNonEmpty deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

type CoreArgs = OutputArgs

type FormatArgs = OutputArgs

data OutputArgs = OA Bool Bool [String]

rmdir :: Utility RmdirSwitch IOArgs CoreArgs FormatArgs OutputArgs
rmdir = Utility "rmdir" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = OA (gotArg args $ Ext IgnoreFailOnNonEmpty)
        (gotArg args $ Std Pl) (argsRest args)

    core = id

    format = id

    output (OA ignoreNonEmpty removeParents paths) = forM_ paths $ \path -> do
        isEmpty <- isDirectoryEmpty path
        if' isEmpty
            (mapM_ removeDirectory removableDirectories)
            actionOnNonEmpty
        where removableDirectories = if' removeParents [] paths
              actionOnNonEmpty =
                  unless ignoreNonEmpty (error "Target must be empty")

argDescs :: [Arg (Switch RmdirSwitch)]
argDescs =
    [ mkExt IgnoreFailOnNonEmpty "ignore-fail-on-non-empty"
          ("ignore each failure that is solely because a directory\n" ++
           "non-empty")
    , mkNamed Pl "parents"
          ("remove DIRECTORY and its ancestors; e.g., `rmdir -p a/b/c' is\n" ++
           "similar to `rmdir a/b/c/ a/b a")
    , mkNamed Vl "verbose" "output a diagnostic for every directory processed"
    , mkHelp
    , mkVersion
    ]

-- Output Helpers -------------------------------------------------------------

isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty =
    fmap (not . any (\c -> c /= "." && c /= "..")) . getDirectoryContents
