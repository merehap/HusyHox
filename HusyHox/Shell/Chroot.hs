module HusyHox.Shell.Chroot (main, chroot) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness chroot

data ChrootSwitch = Userspec | Groups deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

chroot :: Utility ChrootSwitch IOArgs CoreArgs FormatArgs OutputArgs
chroot = Utility "chroot" argDescs input parse core format output where
    input _ = return $ IA ""

    parse _ (IA text) = CA text

    core (CA text) = FA text

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch ChrootSwitch)]
argDescs =
    [ mkExtData Userspec "userspec" "USER:GROUP"
          "specify user and group (ID or name) to use"
    , mkExtData Groups "groups" "G_LIST"
          "specify supplementray groups as g1,g2,..,gN"
    , mkHelp
    , mkVersion
    ]
