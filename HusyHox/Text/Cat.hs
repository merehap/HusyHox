module HusyHox.Text.Cat (main, cat) where

import Data.List (group, intercalate)

import HusyHox.Common.Extra (if', readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness cat

data CatSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA LineNumbering Bool Bool Bool Bool [String]

data LineNumbering = None | NonBlankOnly | All

data FormatArgs = FA [String]

data OutputArgs = OA String

cat :: Utility CatSwitch IOArgs CoreArgs FormatArgs OutputArgs
cat = Utility "cat" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse args (IA inputs) = CA numbering collapse endings tabs specials inputs
        where numbering = case (check [Nl], check [Bl]) of
                  (_   , True) -> NonBlankOnly
                  (True, _   ) -> All
                  (_   , _   ) -> None
              collapse = check [Sl]
              endings  = check [A, El, E]
              tabs     = check [A, Tl, T]
              specials = check [A, El, Tl, Vl]
              check    = any (gotStdArg args)

    core (CA numbering collapse ends tabs specials inputs) = FA $
        map (intercalate "\n" . foldTransformers  . lines2) inputs
        where foldTransformers = flip (foldr ($)) transformers
              showNumbering = number numbering
              transformers = zipWith (\x y -> if' x y id)
                  [ends    , True         , collapse, tabs    , specials    ]
                  [showEnds, showNumbering, squeeze , showTabs, showSpecials]

    format (FA xs) = OA $ concat xs

    output (OA text) = putStr text

argDescs :: [Arg (Switch CatSwitch)]
argDescs =
    [ mkNamed    A  "show-all"        "equivalent to -vET"
    , mkNamed    Bl "number-nonblank" "number nonempty output lines"
    , mkAbbrOnly El                   "equivalent to -vE"
    , mkNamed    E  "show-ends"       "display $ at end of each line"
    , mkNamed    Nl "number"          "number all output lines"
    , mkNamed   Sl "squeeze-blank" "suppress repeated empty output lines"
    , mkAbbrOnly Tl                   "equivalent to -vT"
    , mkNamed    T  "show-tabs"       "display TAB characters as ^I"
    , mkAbbrOnly Ul                   "(ignored)"
    , mkNamed    Vl "show-nonprinting"
                               "use ^ and M- notation, except for LFD and TAB"
    , mkHelp
    , mkVersion
    ]

-- LINE TRANSFORMERS----------------------------------------------------------

squeeze :: [String] -> [String]
squeeze = concatMap (\g -> if' (null $ head g) [""] g) . group

-- The cat reference implemention doesn't number the final line if it is empty
-- This results in the 'All' case below being much uglier than it should be
number :: LineNumbering -> [String] -> [String]
number _         [] = []
number numbering xs = case numbering of
    None         -> xs
    NonBlankOnly -> numberNonBlank 1 xs
    All          -> zipWith numberLine [1..] (init xs)
                 ++ if' (null l) [l] [numberLine (length xs) l]
    where l = last xs

showEnds :: [String] -> [String]
showEnds [] = []
showEnds xs = map (++ "$") (init xs) ++ [last xs]

showTabs :: [String] -> [String]
showTabs = map (concatMap (\c -> if' (c == '\t') "^I" [c]))

showSpecials :: [String] -> [String]
showSpecials = map (concatMap convertSpecial)
    where convertSpecial c = case c of
              '\0' -> "^@"
              '\a' -> "^G"
              '\b' -> "^H"
              '\f' -> "^L"
              '\r' -> "^M"
              '\v' -> "^K"
              x    -> [x]

-- HELPERS--------------------------------------------------------------------

numberNonBlank :: Int -> [String] -> [String]
numberNonBlank _ []     = []
numberNonBlank n (x:xs) = x' : numberNonBlank n' xs
    where (n', x') = if' (null x) (n, x) (n + 1, numberLine n x)

numberLine :: Int -> String -> String
numberLine n l = concat ["     ", show n, "\t", l]

lines2 :: String -> [String]
lines2 xs = lines xs ++ if' (last xs == '\n') [""] []
