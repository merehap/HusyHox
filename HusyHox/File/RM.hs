module HusyHox.File.RM (main, rm) where

import System.Directory (removeFile)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness rm

data RMSwitch = Interactive | OneFileSystem | NoPreserveRoot | PreserveRoot
     deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data CoreArgs = CA [String]

data FormatArgs = FA [String]

type OutputArgs = [String]

rm :: Utility RMSwitch IOArgs CoreArgs FormatArgs OutputArgs
rm = Utility "rm" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = CA $ argsRest args

    core (CA paths) = FA paths

    format (FA paths) = paths

    output = mapM_ removeFile

argDescs :: [Arg (Switch RMSwitch)]
argDescs =
    [ mkNamed Fl "force" "ignore nonexistent files, never prompt"
    , mkAbbrOnly Il "prompt before every removal"
    , mkAbbrOnly I
          ("prompt once before removing more than three files, or\n" ++
           "  when removing recursively. Less intrusive than -i,\n" ++
           "  while still giving protection against most mistakes")
    , mkExtData Interactive "interactive" "WHEN"
          ("prompt according to WHEN: never, once (-I), or\n" ++
           "always (-i). Without WHEN, prompt always")
    , mkExt OneFileSystem "one-file-system"
          ("when removing a hierarchy recursively, skip any\n" ++
           "  directory that is on a file system different from\n" ++
           "  that of the corresponding command line argument")
    , mkExt NoPreserveRoot "no-preserve-root" "do not treat `/' specially"
    , mkExt PreserveRoot "preserve-root" "do not remove `/' (default)"
    , mkNamed Rl "recursive"
          "remove directories and their contents recursively"
    , mkNamed Vl "verbose" "explain what is being done"
    , mkHelp
    , mkVersion
    ]
