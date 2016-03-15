module HusyHox.File.DF (main, df) where

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness df

data DFSwitch = Total | NoSync | Sync deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

df :: Utility DFSwitch IOArgs CoreArgs FormatArgs OutputArgs
df = Utility "df" argDescs input parse core format output where
    input _ = return $ IA ""

    parse _ (IA text) = CA text

    core (CA text) = FA text

    format (FA text) = OA text

    output (OA text) = putStr text

argDescs :: [Arg (Switch DFSwitch)]
argDescs =
    [ mkNamed Al "all" "include dummy file systems"
    , mkNamedData B  "block-size" "SIZE" "use SIZE-byte blocks"
    , mkExt Total "total" "produce a grand total"
    , mkNamed Hl "human-readable"
          "print sizes in human readable format (e.g., 1K 234M 2G)"
    , mkNamed H "si" "likewise, but use powers of 1000 not 1024"
    , mkNamed Il "inodes" "list inode information instead of block usage"
    , mkAbbrOnly Kl "like --block-size=1K"
    , mkNamed Ll "local" "limit listing to local file systems"
    , mkExt NoSync "no-sync"
          "do not invoke sync before getting usage info (default)"
    , mkNamed P "portability" "use the POSIX output format"
    , mkExt Sync "sync" "invoke synx before getting usage info"
    , mkNamedData Tl "type" "TYPE" "limit listing to file systems of type TYPE"
    , mkNamed T "print-type" "print file system type"
    , mkNamedData Xl "exclude-type" "TYPE"
          "limit listing to file systems not of type TYPE"
    , mkAbbrOnly Vl "(ignored)"
    , mkHelp
    , mkVersion
    ]
