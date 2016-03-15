module HusyHox.Shell.Printenv (main, printenv) where

import System.Environment (getEnvironment)

import HusyHox.Common.Environment (formatEnvVar)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness printenv

data PrintenvSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data CoreArgs = CA [(String,String)]

data FormatArgs = FA [String]

data OutputArgs = OA String

printenv :: Utility PrintenvSwitch CoreArgs CoreArgs FormatArgs OutputArgs
printenv = Utility "printenv" argDescs input parse core format output where
    input _ = fmap CA getEnvironment

    parse _ = id

    core (CA vars) = FA $ map (uncurry formatEnvVar) vars

    format (FA lns) = OA $ unlines lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch PrintenvSwitch)]
argDescs =
    [ mkNamed Zero "null" "end each output line with 0 byte rather than newline"
    , mkHelp
    , mkVersion
    ]
