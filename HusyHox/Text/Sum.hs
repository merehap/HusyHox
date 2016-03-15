module HusyHox.Text.Sum (main, sum) where

import Prelude hiding (sum)

import Control.Arrow ((&&&))
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Word (Word16, Word32)

import HusyHox.Common.Extra (formatSummary, if', readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness sum

data SumSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data Algorithm = Bsd | SysV

data IOArgs = IA [String]

data CoreArgs = CA Algorithm [String]

data FormatArgs = FA [(Word16, Int)]

data OutputArgs = OA String

sum :: Utility SumSwitch IOArgs CoreArgs FormatArgs OutputArgs
sum = Utility "sum" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse args (IA texts) = CA algorithm texts
        where algorithm = if' (gotStdArg args Sl) SysV Bsd

    core (CA algorithm texts) = FA $ calculate $ case algorithm of
        Bsd  -> (bsdChecksum , bsdBlockWidth )
        SysV -> (sysVChecksum, sysVBlockWidth)
        where calculate (algo, width) =
                  map ((algo &&& blockCount width) . stringToWords) texts
              blockCount width = (`div` width) . (+ (width - 1)) . length

    format (FA ns) = OA $ formatSummary ns

    output (OA text) = putStr text

argDescs :: [Arg (Switch SumSwitch)]
argDescs =
    [ mkAbbrOnly Rl "use BSD sum algorithm, use 1K blocks"
    , mkNamed    Sl "sysv"
          "use System V sum algorithm, use 512 bytes blocks"
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

bsdBlockWidth :: Int
bsdBlockWidth = 1024

sysVBlockWidth :: Int
sysVBlockWidth = 512

bsdChecksum :: [Word16] -> Word16
bsdChecksum = fromIntegral . worker 0 . map fromIntegral
    where worker :: Word32 -> [Word32] -> Word32
          worker cs []     = cs
          worker cs (x:xs) = worker (fromIntegral cs''') xs
              where cs'   = (cs `shiftR` 1) + ((cs .&. 1) `shiftL` 15)
                    cs''  = cs' + x
                    cs''' = cs'' .&. 0xffff

sysVChecksum :: [Word16] -> Word16
sysVChecksum = worker (0 :: Word32)
    where worker cs []     = fromIntegral cs
          worker cs (x:xs) = worker (cs + fromIntegral x) xs

stringToWords :: String -> [Word16]
stringToWords []       = []
stringToWords [x]      = [fromIntegral $ promote x]
stringToWords (x:y:zs) =
    fromIntegral (promote x + fromEnum y) : stringToWords zs

promote :: Char -> Int
promote = flip shiftL 8 . fromEnum
