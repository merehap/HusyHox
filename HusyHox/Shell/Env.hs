module HusyHox.Shell.Env (main, env) where

import System.Environment (getEnvironment)

import HusyHox.Common.Environment (formatEnvVar)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness env

data EnvSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [(String, String)]

data CoreArgs = CA [(String, String)]

data FormatArgs = FA [String]

data OutputArgs = OA String

env :: Utility EnvSwitch IOArgs CoreArgs FormatArgs OutputArgs
env = Utility "env" argDescs input parse core format output where
    input _ = fmap IA getEnvironment

    parse _ (IA vars) = CA vars

    core (CA vars) = FA $ map (uncurry formatEnvVar) vars

    format (FA lns) = OA $ unlines lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch EnvSwitch)]
argDescs =
    [ mkNamed Il "ignore-envrionment" "start with an empty environment"
    , mkNamed Zero "null"
          "end each output line with 0 byte rather than newline"
    , mkNamedData Ul "unset" "NAME" "remove variable from the environment"
    , mkHelp
    , mkVersion
    ]
