module HusyHox.File.Mkdir (main, mkdir) where

import System.Directory (createDirectory)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness mkdir

data MkdirSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data CoreArgs = CA [String]

data FormatArgs = FA [String]

data OutputArgs = OA [String]

mkdir :: Utility MkdirSwitch IOArgs CoreArgs FormatArgs OutputArgs
mkdir = Utility "mkdir" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = CA $ argsRest args

    core (CA dirs) = FA dirs

    format (FA dirs) = OA dirs

    output (OA dirs) = mapM_ createDirectory dirs

argDescs :: [Arg (Switch MkdirSwitch)]
argDescs =
    [ mkNamedData Ml "mode" "MODE"
          "set file mode (as in chmod), not a=rwx - umask"
    , mkNamed Pl "parents"
          "no error if existing, make parent directories as needed"
    , mkNamed Vl "verbose" "print a message for each created directory"
    , mkNamedData Z "context" "CTX"
          ("set the SELinux security context of each created\n" ++
           "directory to CTX")
    , mkHelp
    , mkVersion
    ]
