module HusyHox.Text.Cut (main, cut) where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe)

import HusyHox.Common.Extra (readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness cut

data CutSwitch = Complement | OutputDelimiter deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [[String]]

data CoreArgs = CA AtomType [(Int, Int)] [[String]]

data AtomType = Byte | Char | Field

data FormatArgs = FA [[String]]

data OutputArgs = OA String

cut :: Utility CutSwitch IOArgs CoreArgs FormatArgs OutputArgs
cut = Utility "cut" argDescs input parse core format output where
    input = fmap (IA . map lines) . readInputs . argsRest

    parse args (IA lns) = CA atom ranges lns
        where (atom, ranges) = fromMaybe
                  (error "Must specify either -b, -c, or -f.")
                  (getAtomInfo args)

    core (CA _ ranges lns) = FA $ map (map (getAtoms mask)) lns
        where mask = getMask ranges

    format (FA lns) = OA $ concatMap unlines lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch CutSwitch)]
argDescs =
    [ mkNamedData Bl "bytes"      "LIST"  "select only these bytes"
    , mkNamedData Cl "characters" "LIST"  "select only these characters"
    , mkNamedData Dl "delimiter"  "DELIM"
          "use DELIM instead of TAB for field delimiter"
    , mkNamedData Fl "fields"     "LIST"
          ("select only these fields; also print any line that contains" ++
           " no delimiter character, unless the -s option is specified")
    , mkAbbrOnly Nl "with -b: don't split mutibyte characters"
    , mkExt Complement "complement"
          "complement the set of selected bytes, character or fields"
    , mkNamed Sl "only-delimited"
          "do not print lines not containing delimiters"
    , mkExtData OutputDelimiter "output-delimiter" "STRING"
          "use STRING as the output delimiter"
    , mkHelp
    , mkVersion
    ]

-- PARSE ----------------------------------------------------------------------

getAtomInfo :: Args (Switch CutSwitch) -> Maybe (AtomType, [(Int, Int)])
getAtomInfo args = case selected of
    [(atom, rangeText)] -> Just (atom, parseRanges rangeText)
    _                   -> Nothing
    where selected   = catMaybes $ zipWith (fmap . (,)) atoms rangeTexts
          rangeTexts = map (getStdArg args) [Bl, Cl, Fl]
          atoms      = [Byte, Char, Field]

parseRanges :: String -> [(Int, Int)]
parseRanges = map parseRange . splitOn ","

parseRange :: String -> (Int, Int)
parseRange text
    | '-' `notElem` text = (\x -> (x,x)) $ read text
    | head text == '-' = (0, read $ tail text)
    | last text == '-' = (read $ init text, length text - 1)
    | otherwise = (read (head xs), read (xs !! 1))
          where xs = splitOn "-" text

-- CORE -----------------------------------------------------------------------

getAtoms :: [Bool] -> [a] -> [a]
getAtoms mask = map snd . filter fst . zip mask

getMask :: [(Int, Int)] -> [Bool]
getMask = foldr getRangeMask initial
    where initial = repeat False

getRangeMask :: (Int, Int) -> [Bool] -> [Bool]
getRangeMask (start, end) = zipWith (||) mask
    where mask = concat
             [ replicate start             False
             , replicate (end - start + 1) True
             , repeat                      False
             ]
