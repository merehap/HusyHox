module HusyHox.Text.WC (main, wc) where

import Control.Arrow (second)
import Data.List (genericLength, transpose)

import HusyHox.Common.Arg
import HusyHox.Common.Extra (if', readInput, readInputs)
import HusyHox.Common.Format
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness wc

data WCSwitch = Files0From deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String] [String]

data CoreArgs = CA Bool Bool Bool Bool Bool [(String, String)]

data FormatArgs = FA [(String, [Integer])] [Integer]

data OutputArgs = OA String

wc :: Utility WCSwitch IOArgs CoreArgs FormatArgs OutputArgs
wc = Utility "wc" argDescs input parse core format output where
    input args = do
        let standardPaths = argsRest args
            extraPath     = getArg args $ Ext Files0From
        paths <- getFilePaths standardPaths extraPath
        texts <- readInputs paths
        return $ IA paths texts

    parse args (IA paths texts) = CA b c w l m pathTexts
        where switchFlags     = map (gotArg args . Std) switches
              [b, c, w, l, m] = if' (or switchFlags) switchFlags defaults
              pathTexts       = zip paths texts

    core (CA b c w l m files) = FA allStats totals
        where allStats        = map (second stats) files
              totals          = (map sum . transpose . map snd) allStats
              stats contents  = map ($ contents) currentCounters
              currentCounters =
                  map snd . filter fst $ zip [b, c, w, l, m] counters

    format (FA []     _     ) = OA []
    format (FA xs@(_:xs') totals) = OA $ unlines $ map formatLine lns
        where indices = getRightIndents (snd $ last lns)
              formatLine (name, stats) = concat
                  [ formatRightIndents indices stats, " ", name]
              lns = map (second (map show))
                  (xs ++ if' (null xs') [] [("total", totals)])

    output (OA text) = putStr text

argDescs :: [Arg (Switch WCSwitch)]
argDescs =
    [ mkNamed Cl "bytes" "Print the byte counts."
    , mkNamed Ml "chars" "Print the character counts."
    , mkNamed Wl "words" "Print the word counts."
    , mkNamed Ll "lines" "Print the newline counts."
    , mkNamed L  "max-line-length" "Print the maximum line length."
    , mkExtData Files0From "files0-from" "file"
          "Use files in 'file' rather than from the command line."
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

defaults :: [Bool]
switches :: [StdSwitch]
counters :: [String -> Integer]
(defaults, switches, counters) = unzip3
    [ (True , Ll, elemCount '\n'                     )
    , (True , Wl, genericLength . words              )
    , (True , Ml, genericLength                      )
    , (False, Cl, genericLength                      )
    , (False, L , maximum . map genericLength . lines)
    ]

getFilePaths :: [String] -> Maybe String -> IO [String]
getFilePaths []      Nothing = return ["-"]
getFilePaths paths   extra   = fmap (++ paths) extraPaths
    where extraPaths = maybe (return []) (fmap lines . readInput . Just) extra

elemCount :: (Eq a) => a -> [a] -> Integer
elemCount _ [] = 0
elemCount x (y:ys) = if' (x == y) 1 0 + elemCount x ys
