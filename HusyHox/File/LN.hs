module HusyHox.File.LN (main, ln) where

import System.Directory (getCurrentDirectory)
import System.FilePath (combine)
import System.Posix.Files (createSymbolicLink)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness ln

data LNSwitch = Backup deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

type CoreArgs = OutputArgs

type FormatArgs = OutputArgs

data OutputArgs = OA [(String, String)]

ln :: Utility LNSwitch IOArgs CoreArgs FormatArgs OutputArgs
ln = Utility "ln" argDescs input parse core format output where
    input _ = fmap IA getCurrentDirectory

    parse args (IA cd) = OA $ case (target, directory, argsRest args) of
        (Just _ , _      , [] ) -> error "Link name must be specified with -T"
        (Just t , _      , [x]) -> [(t, x)]
        (Just _ , _      , _  ) -> error "Only one link must be specified"
        (_      , Just _ , [] ) -> error "Targets must be specified"
        (_      , Just d , xs ) -> map (\t -> (d `combine` t, t)) xs
        (_      , Nothing, [] ) -> error "Targets must be specified"
        (_      , Nothing, [t]) -> [(t, cd)]
        (_      , Nothing, xs ) -> let (ts, [d]) = splitAt (length xs - 1) xs
                                   in map (\t -> (d `combine` t, t)) ts
        where target = getArg args $ Std Tl
              directory = getArg args $ Std T

    core = id

    format = id

    output (OA links) = mapM_ (uncurry createSymbolicLink) links

argDescs :: [Arg (Switch LNSwitch)]
argDescs =
    [ mkExt Backup "backup" "make a backup of each exisitng destination file"
    , mkAbbrOnly Bl "like --backup but does not accept an argument"
    , mkNamed Dl "directory" "allow the superuser to attempt to hard link"
    , mkNamed Fl "force" "remove existing destination files"
    , mkNamed Il "interactive" "prompt whether to remove destinations"
    , mkNamed L "logical" "make hard links to symbolic link reference"
    , mkNamed Nl "no-dereference"
          ("treat destination that is a symlink to a\n" ++
           "directory as if it were a normal file")
    , mkNamed P "physical" "make hard links directly to symbolic links"
    , mkNamed Sl "symbolic" "make symbolic links instead of hard links"
    , mkNamedData S "suffix" "SUFFIX" "override the usual backup suffix"
    , mkNamedData Tl "target-directory" "DIRECTORY"
          ("specify the DIRECTORY in which to create\n" ++
           "the links")
    , mkNamed T "no-target-directory" "treat LINK_NAME as a normal file"
    , mkNamed Vl "verbose" "print name of each linked file"
    , mkHelp
    , mkVersion
    ]
