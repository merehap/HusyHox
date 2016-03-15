module HusyHox.File.Shred (main, shred) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness shred

data ShredSwitch = RandomSource deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

shred :: Utility ShredSwitch IOArgs CoreArgs FormatArgs OutputArgs
shred = Utility "shred" argDescs input parse core format output where
    input _ = return $ IA ""

    parse _ (IA text) = CA text

    core (CA text) = FA text

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch ShredSwitch)]
argDescs =
    [ mkNamed Fl "force" "change permissions to allow writing if necessary"
    , mkNamedData Nl "iterations" "N"
          "overwrite N times instead of the default (3)"
    , mkExtData RandomSource "random-source" "FILE"
          "get random bytes from FILE"
    , mkNamedData Sl "size" "N"
          "shred this many bytes (suffixes like K, M, G accepted)"
    , mkNamed Ul "remove" "truncated and remove file after overwriting"
    , mkNamed Vl "verbose" "show progress"
    , mkNamed Xl "exact"
          ("do not round file sizes up to the next full block;" ++
           "  this is the default for non-regular files")
    , mkNamed Zl "zero" "add a final overwrite with zeros to hide shredding"
    , mkHelp
    , mkVersion
    ]
