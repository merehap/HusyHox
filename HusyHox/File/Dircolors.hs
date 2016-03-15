module HusyHox.File.Dircolors (main, dircolors) where

import System.Environment (getEnv)

import HusyHox.Common.Extra (if')
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness dircolors

data DirColors = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data Shell = Csh | SH

data CoreArgs = CA Shell String

data OutputArgs = OA String

dircolors :: Utility DirColors IOArgs CoreArgs OutputArgs OutputArgs
dircolors = Utility "dircolors" argDescs input parse core format output where
    input _ = fmap IA $ getEnv "LS_COLORS"

    parse args (IA text) = CA shell text
        where shell = if' (gotArg args $ Std Cl) Csh SH

    core (CA shell text) = OA $ concat [start, text, end]
        where (start, end) = case shell of
                  Csh -> ("setenv LS_COLORS '", "'")
                  SH  -> ("LS_COLORS='", "';" ++ "\n"++ "export LS_COLORS")

    format = id

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch DirColors)]
argDescs =
    [ mkNamed Bl "sh" "output Bourne shell code to set LS_COLORS"
    , mkNamed Cl "csh" "output C shell code to set LS_COLORS"
    , mkNamed Pl "print-database" "output defaults"
    , mkHelp
    , mkVersion
    ]
