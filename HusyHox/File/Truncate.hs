module HusyHox.File.Truncate (main, truncate) where

import Prelude hiding (truncate)

import Control.Monad (forM_, unless)
import System.Directory (doesFileExist)
import System.Posix.Files (setFileSize)
import System.Posix.Types (COff)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness truncate

data TruncateSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data OutputArgs = OA COff [String]

truncate :: Utility TruncateSwitch IOArgs OutputArgs OutputArgs OutputArgs
truncate = Utility "truncate" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = OA (read $ getRequiredArg args $ Std Sl) (argsRest args)

    core = id

    format = id

    output (OA size paths) = forM_ paths $ \path -> do
        exists <- doesFileExist path
        unless exists (writeFile path "")
        setFileSize path size

argDescs :: [Arg (Switch TruncateSwitch)]
argDescs =
    [ mkNamed Cl "no-create" "do not create any files"
    , mkNamed Ol "io-blocks"
          "treat SIZE as number of IO blocks instead of bytes"
    , mkNamedData Rl "reference" "FILE" "use this FILE's size"
    , mkRequiredNamedData Sl "size" "SIZE" "use this SIZE"
    , mkHelp
    , mkVersion
    ]
