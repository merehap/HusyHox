module HusyHox.Shell.Printf (main, printf) where

import HusyHox.Common.Format (compressEscapeCharacters)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness printf

data PrintfSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

printf :: Utility PrintfSwitch IOArgs CoreArgs FormatArgs OutputArgs
printf = Utility "printf" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = CA $ getRequiredStdArg args Unnamed

    core (CA text) = FA $ compressEscapeCharacters text

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch PrintfSwitch)]
argDescs =
    [ mkRequiredUnnamed Unnamed "FORMAT" "FORMAT"
    ]
