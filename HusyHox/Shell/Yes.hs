module HusyHox.Shell.Yes (main, yes) where

import HusyHox.Common.Extra (getStdArgOrDefault)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness yes

data YesSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data CoreArgs = CA String

data OutputArgs = OA String

yes :: Utility YesSwitch IOArgs CoreArgs OutputArgs OutputArgs
yes = Utility "yes" argDescs input parse core format output where
    input _ =  return $ IA ()

    parse args _ = CA (getStdArgOrDefault "y" args Unnamed)

    core (CA text) = (OA . concat . repeat . (++ "\n"))  text

    format = id

    output (OA text) = putStr text

argDescs :: [Arg (Switch YesSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    , mkUnnamed Unnamed "VALUE" "VALUE"
    ]
