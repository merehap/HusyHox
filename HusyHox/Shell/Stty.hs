module HusyHox.Shell.Stty (main, stty) where

import System.Posix.IO (stdInput)
import System.Posix.Terminal (BaudRate(..), getTerminalAttributes, inputSpeed)
import Text.Printf (printf)

import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness stty

data SttySwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA BaudRate

data CoreArgs = CA BaudRate

data FormatArgs = FA BaudRate

data OutputArgs = OA String

stty :: Utility SttySwitch IOArgs CoreArgs FormatArgs OutputArgs
stty = Utility "stty" argDescs input parse core format output where
    input _ = do
        attributes <- getTerminalAttributes stdInput
        return (IA $ inputSpeed attributes)

    parse _ (IA speed) = CA speed

    core (CA speed) = FA speed

    format (FA speed) = (OA . printf fmt . show . baudRateToInt) speed
        where fmt = "speed %s baud; line = dummy;\n-dummy -dummy dummy"

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch SttySwitch)]
argDescs =
    [ mkNamed Al "all"  "print all current settings in human-readable form"
    , mkNamed Gl "save" "print all current settings in a stty-readable form"
    , mkNamedData Fl "file" "DEVICE"
          "open and use the specified DEVICE instead of stdin"
    , mkHelp
    , mkVersion
    ]

-- Format Helpers -------------------------------------------------------------

baudRateToInt :: BaudRate -> Int
baudRateToInt baudRate = case baudRate of
    B0      -> 0
    B50     -> 50
    B75     -> 75
    B110    -> 110
    B134    -> 134
    B150    -> 150
    B200    -> 200
    B300    -> 300
    B600    -> 600
    B1200   -> 1200
    B1800   -> 1800
    B2400   -> 2400
    B4800   -> 4800
    B9600   -> 9600
    B19200  -> 19200
    B38400  -> 38400
    B57600  -> 57600
    B115200 -> 115200
