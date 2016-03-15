module HusyHox.Shell.Tee (main, tee) where


import HusyHox.Common.Extra (if', modifyOutput)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness tee

data TeeSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data OutputArgs = OA Bool String [String]

tee :: Utility TeeSwitch IOArgs OutputArgs OutputArgs OutputArgs
tee = Utility "tee" argDescs input parse core format output where
    input _ = fmap IA getContents

    parse args (IA text) = OA append text files
        where files  = argsRest args
              append = gotStdArg args Al

    core = id

    format = id

    output (OA append text paths) = mapM_ (flip write text . Just) paths
        where write = modifyOutput $ if' append appendFile writeFile

argDescs :: [Arg (Switch TeeSwitch)]
argDescs =
    [ mkNamed Al "append" "append to the given FILEs, do not overwrite"
    , mkNamed Il "ignore-interrupts" "ignore interrupt signals"
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "FILE" "FILE"
    ]
