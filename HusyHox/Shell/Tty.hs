module HusyHox.Shell.Tty (main, tty) where

import Control.Monad (unless)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (getTerminalName)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness tty

data TtySwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA Bool String

tty :: Utility TtySwitch OutputArgs OutputArgs OutputArgs OutputArgs
tty = Utility "tty" argDescs input parse core format output where
    input args = do
        let silent = gotArg args (Std Sl)
        terminalName <- getTerminalName stdInput
        return $ OA silent terminalName

    parse _ = id

    core = id

    format = id

    output (OA silent text) = unless silent (putStrLn text)

argDescs :: [Arg (Switch TtySwitch)]
argDescs =
    [ mkNamed Sl "silent" "print nothing, only return an exit status"
    , mkHelp
    , mkVersion
    ]
