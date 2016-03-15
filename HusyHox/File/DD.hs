module HusyHox.File.DD (main, dd) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness dd

data DDSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

dd :: Utility DDSwitch IOArgs CoreArgs FormatArgs OutputArgs
dd = Utility "dd" argDescs input parse core format output where
    input _ = return $ IA ""

    parse _ (IA text) = CA text

    core (CA text) = FA text

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch DDSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
