module HusyHox.Shell.Dirname (main, dirname) where

import System.FilePath.Posix (takeDirectory)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness dirname

data DirnameSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

dirname :: Utility DirnameSwitch IOArgs CoreArgs FormatArgs OutputArgs
dirname = Utility "dirname" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = CA $ getRequiredStdArg args Unnamed

    core (CA path) = FA $ if' ('/' `elem` path) takeDirectory (const ".") path

    format (FA path) = OA (path ++ "\n")

    output (OA text) = putStr text

argDescs :: [Arg (Switch DirnameSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    , mkRequiredUnnamed Unnamed "NAME" "NAME"
    ]
