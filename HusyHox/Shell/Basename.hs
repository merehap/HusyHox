module HusyHox.Shell.Basename (main, basename) where

import Data.List.Utils (endswith)
import System.FilePath (takeFileName)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness basename

data BasenameSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data CoreArgs = CA String (Maybe String)

data FormatArgs = FA String

data OutputArgs = OA String

basename :: Utility BasenameSwitch IOArgs CoreArgs FormatArgs OutputArgs
basename = Utility "basename" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = CA path suffix
        where path   = getRequiredStdArg args Unnamed
              suffix = getStdArg args Unnamed2

    core (CA path maybeSuffix) = FA $ maybe id dropSuffix maybeSuffix fileName
        where fileName = takeFileName path
              dropSuffix suffix value =
                  if' (value `endswith` suffix)
                      (drop (length value - length suffix) value)
                      value

    format (FA text) = OA text

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch BasenameSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    , mkRequiredUnnamed Unnamed "NAME" "NAME"
    , mkUnnamed Unnamed2 "SUFFIX" "SUFFIX"
    ]
