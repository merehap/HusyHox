-- Adapted from Chris Johnson's code

module HusyHox.Text.Tsort (main, tsort) where

import Data.List (nub)

import HusyHox.Common.Extra (readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness tsort

data TsortSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA [[String]]

data FormatArgs = FA [String]

data OutputArgs = OA String

tsort :: Utility TsortSwitch IOArgs CoreArgs FormatArgs OutputArgs
tsort = Utility "tsort" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse _ (IA texts) = CA $ map lines texts

    core (CA lns) = FA $ tsort' lns

    format (FA lns) = OA $ unlines lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch TsortSwitch)]
argDescs =
    [ mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

tsort' :: Eq a => [[a]] -> [a]
tsort' [] = []
-- Drop empty lists
tsort' seqs =
    let seqs' = filter (not . null) seqs
        heads = nub (map head seqs')
        -- Tails of sequences are inaccessible
        restricted = nub (tail =<< seqs') -- (=<<) acts like concatMap
        -- Heads that are not restricted are just grand
        winners = filter (not . contains restricted) heads
        -- Remove any winners from the beginning of our sequences
        losers = map (dropWhile (contains winners)) seqs'
    in if null winners && (not . null) losers
       then error "No ordering exists"
       else winners ++ tsort' losers

contains :: Eq a => [a] -> a -> Bool
contains = flip elem
