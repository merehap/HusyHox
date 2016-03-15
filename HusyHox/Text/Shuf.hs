module HusyHox.Text.Shuf (main, shuf) where

import System.Random (newStdGen, randomR, StdGen)

import HusyHox.Common.Extra
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness shuf

data ShufSwitch = RandomSource deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA StdGen [String]

data FormatArgs = FA [String]

data OutputArgs = OA String

shuf :: Utility ShufSwitch IOArgs IOArgs FormatArgs OutputArgs
shuf = Utility "shuf" argDescs input parse core format output where
    input args = do
        lns <- fmap lines (readInput $ getArg args $ Std Unnamed)
        gen <- newStdGen
        return $ IA gen lns

    parse _ = id

    core (IA gen lns) = FA $ randomPermute gen lns

    format (FA lns) = OA $ unlines lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch ShufSwitch)]
argDescs =
    [ mkNamed El "echo" "treat each ARG as an input line"
    , mkNamedData       Il "input-range" "LO-HI"
          "treat each number LO through HI as an input line"
    , mkNamedData       Nl "head-count" "COUNT" "output at most COUNT lines"
    , mkNamedData       Ol "output" "FILE"
          "write result to FILE instead of standard output"
    , mkExtData         RandomSource "random-source" "FILE"
          "get random bytes from FILE"
    , mkNamed Zl "zero-terminated" "end lines with 0 byte, not newline"
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "FILE" "FILE"
    ]

-- Helpers --------------------------------------------------------------------

randomPermute :: StdGen -> [a] -> [a]
randomPermute gen xs = worker gen (length xs) xs
    where worker _ 0 xs' = xs'
          worker g c xs' = x : worker ng (c - 1) nxs
              where (n, ng)  = randomR (0, c - 1) g
                    (x, nxs) = extract n xs'

extract :: Int -> [a] -> (a, [a])
extract n xs = (xs !! n, take n xs ++ drop (n + 1) xs)
