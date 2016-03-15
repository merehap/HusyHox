module HusyHox.Text.Fmt (main, fmt) where

import Data.Function (on)
import Data.List (groupBy)
import Data.List.Split (condense, keepDelimsL, oneOf, split)

import HusyHox.Common.Extra (if', readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness fmt

data FmtSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA [String]

data FormatArgs = FA [String]

data OutputArgs = OA String

data LayoutElement = BlankLines Int | Paragraph [String]

fmt :: Utility FmtSwitch IOArgs CoreArgs FormatArgs OutputArgs
fmt = Utility "fmt" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse _ (IA texts) = CA texts

    core (CA texts) = FA $ map (formatLayoutElements . parseLayoutElements) texts

    format (FA texts) = OA $ concat texts

    output (OA text) = putStr text

argDescs :: [Arg (Switch FmtSwitch)]
argDescs =
    [ mkNamed     Cl "crown-margin" "preserve indentation of first two lines"
    , mkNamedData Pl "prefix" "STRING"
          "reformat only lines beginning with STRING"
    , mkNamed     Sl "split-only" "split long lines, but do not refill"
    , mkNamed     Tl "tagged-paragraph"
          "indentation of first line different from second"
    , mkNamed     Ul "uniform-spacing"
          "one space between words, two after sentences"
    , mkNamedData Wl "width" "WIDTH"
          "maximum line width (default of 75 columns)"
    , mkHelp
    , mkVersion
    ]

-- Core Helpers ---------------------------------------------------------------

parseLayoutElements :: String -> [LayoutElement]
parseLayoutElements text = map
    (\xs ->
        if' (null (head xs))
            ((BlankLines . length) xs)
            ((Paragraph . concatMap spaceSplit) xs))
    (groupElements text)
    where groupElements = groupBy ((==) `on` null) . lines
          spaceSplit = split ((keepDelimsL . condense . oneOf) " ")

formatLayoutElements :: [LayoutElement] -> String
formatLayoutElements = concatMap formatLayoutElement

formatLayoutElement :: LayoutElement -> String
formatLayoutElement (BlankLines n) = (concat . replicate n) "\n"
formatLayoutElement (Paragraph xs) = (unlines . concatWords) xs

concatWords :: [String] -> [String]
concatWords = go 0 []
    where go _ [] []     = []
          go _ as []     = [concat as]
          go i as (b:bs) = front ++ go ni buffer rest
              where ei = i + length b + 1
                    (front, ni, buffer, rest) =
                        if' (ei < 75)
                            ([], ei, as ++ [b], bs)
                            ([concat as], 0, [], b:bs)
