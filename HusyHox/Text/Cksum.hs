module HusyHox.Text.Cksum (main, cksum) where

import Control.Arrow ((&&&))
import Data.Hash.CRC32.Posix (crc32)
import Data.Word (Word32)

import HusyHox.Common.Extra (formatSummary, readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness cksum

data CksumSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA [String]

data FormatArgs = FA [(Word32, Int)]

data OutputArgs = OA String

cksum :: Utility CksumSwitch IOArgs CoreArgs FormatArgs OutputArgs
cksum = Utility "cksum" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse _ (IA texts) = CA texts

    core (CA texts) = FA $ map (crc32 &&& length) texts

    format (FA lns) = OA $ formatSummary lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch CksumSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
