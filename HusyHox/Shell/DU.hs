-- Adapted from jmillikin's code
{-# OPTIONS -fspec-constr-count=7 #-}

module HusyHox.Shell.DU (main, du) where

import Control.Monad
import Control.Exception.Extensible
import System.Directory
import System.Posix.Files
import System.FilePath

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness du

data DUSwitch
    = ApparentSize | Files0From | SI | Exclude | MaxDepth | Time | TimeStyle
    deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data CoreArgs = CA String

data FormatArgs = FA String

data OutputArgs = OA String

du :: Utility DUSwitch IOArgs CoreArgs FormatArgs OutputArgs
du = Utility "du" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = CA $ head $ argsRest args

    core (CA path) = FA path

    format (FA path) = OA path

    output (OA filepath) = do
        size <- du' filepath
        print size

argDescs :: [Arg (Switch DUSwitch)]
argDescs =
    [ mkNamed Al "all" "write counts for all files, not just directories"
    , mkExt ApparentSize "apparent-size"
          ("print apparent sizes, rather than disk usage; although\n" ++
           "  the apparent size is usually smaller, it may be\n" ++
           "  larger due to holes in (`sparse') files, internal\n" ++
           "  fragmentation, indirect blocks, and the like\n")
    , mkNamedData B "block-size" "SIZE" "use SIZE-byte blocks"
    , mkNamed Bl "bytes" "equivalent to `--apparent-size --block-size=1"
    , mkNamed Cl "total" "produce a grand total"
    , mkNamed D "dereference-args"
          ("dereference only symlinks that are listed on the\n" ++
           "  command line\n")
    , mkExtData Files0From "files0-from" "F"
          ("summarize disk usage of the NUL-terminated file\n" ++
           "  names specified in file F;\n" ++
           "  If F is - then read names from standard input")
    , mkAbbrOnly H "equivalent to --dereference-args (-D)"
    , mkNamed Hl "human-readable"
          "print sizes in human readable format (e.g., 1K 234M 2G)"
    , mkExt SI "si" "like -h, but use powers of 1000 not 1024"
    , mkAbbrOnly Kl "like --block-size=1K"
    , mkNamed Ll "count-links" "count sizes many times if hard linked"
    , mkAbbrOnly Ml "like --block-size=1M"
    , mkNamed L "dereference"
          "dereference all symbolic links"
    , mkNamed P "no-dereference"
          "don't follow any symbolic links (this is the default)"
    , mkNamed O "null" "end each output line with 0 byte rather than newline"
    , mkNamed S "separate-dirs" "do not include size of subdirectories"
    , mkNamed Sl "summarize" "display only a total for each argument"
    , mkNamed Xl "one-file-system" "skip directories on different file systems"
    , mkNamedData X "exclude-from" "FILE"
          "exclude files that match any pattern in FILE"
    , mkExtData Exclude "exclude" "PATTERN" "exclude files that match PATTERN"
    , mkExtData MaxDepth "max-depth" "N"
          "print the total for a directory (or file, with -all)"
    , mkExtData Time "time" "WORD"
          ("show time as WORD instead of modification time:\n" ++
           "  atime, access, use, ctime or status")
    , mkExtData TimeStyle "time-style" "STYLE"
          ("show times using style STYLE:\n" ++
           "full-iso, long-iso, iso, +FORMAT\n" ++
           "FORMAT is interpreted like `date'")
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

du' :: String -> IO Integer
du' root = foldDirTree step root 0
    where step n path = do
              size <- fileSize' path
              return $! n + size

foldDirTree :: (a -> String -> IO a) -> String -> a -> IO a
foldDirTree k = loop where
    loop dir = foldDir dir $ reject (\e -> e `elem` [".", ".."]) $ \a e -> do
        let path = dir </> e
        isDir <- isDirectory' path
        let next = if isDir then loop path else return
        k a path >>= next

except :: a -> IO a -> IO a
except a = handle (\(SomeException _) -> return a)

foldDir :: String -> (a -> String -> IO a) -> a -> IO a
foldDir path k a = except [] (getDirectoryContents path) >>= foldM k a

fileSize' :: String -> IO Integer
fileSize' = except 0 . fmap (toInteger . fileSize) . getFileStatus

isDirectory' :: String -> IO Bool
isDirectory' = except False . fmap isDirectory . getFileStatus

accept :: Monad m => (b -> Bool) -> (a -> b -> m a) -> a -> b -> m a
accept p k a b = if p b then k a b else return a

reject :: Monad m => (b -> Bool) -> (a -> b -> m a) -> a -> b -> m a
reject p = accept (not . p)
