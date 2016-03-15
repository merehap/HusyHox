module HusyHox.Shell.Factor (main, factor) where

import Control.Arrow ((&&&))
import Data.Numbers (primeFactors)

import HusyHox.Common.Extra (readArgsOrStdin)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness factor

data FactorSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA [Integer]

data FormatArgs = FA [(Integer, [Integer])]

data OutputArgs = OA String

factor :: Utility FactorSwitch IOArgs CoreArgs FormatArgs OutputArgs
factor = Utility "factor" argDescs input parse core format output where
    input = fmap IA . readArgsOrStdin . argsRest

    parse _ (IA xs) = CA $ map read xs

    core (CA xs) = FA $ map (id &&& (reverse . primeFactors)) xs

    format (FA xs) = OA $ unlines $ map (uncurry formatLine) xs
        where formatLine n fs = concat
                  [show n, ": ", unwords $ map show fs]

    output (OA text) = putStr text

argDescs :: [Arg (Switch FactorSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]
