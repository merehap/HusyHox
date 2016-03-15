module HusyHox.Text.Csplit (main, csplit) where

import Data.Char (isDigit)
import Data.Either.Unwrap (fromRight)
import Data.Maybe (fromMaybe)

import HusyHox.Common.Extra (readInput)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness csplit

data CsplitSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA Int String (Maybe [Int]) [String]

data FormatArgs = FA (Maybe [Int]) [(String, String)]

data OutputArgs = OA String [(String, String)]

csplit :: Utility CsplitSwitch IOArgs CoreArgs FormatArgs OutputArgs
csplit = Utility "csplit" argDescs input parse core format output where
    input = fmap IA . readInput . flip getStdArg Unnamed

    parse args (IA text) = CA 2 "xx" (Just counts) splits
        where patterns = map (fromRight . parsePattern) (argsRest args)
              splits = getSplits patterns (lines text)
              counts = map length splits

    core (CA padWidth prefix maybeSizes lns) =
        FA maybeSizes (formatChunks padWidth prefix lns)

    format (FA maybeSizes chunks) = OA (formatSizes maybeSizes) chunks

    output (OA sizeText chunks) = do
        putStrLn sizeText
        mapM_ (uncurry writeFile) chunks

argDescs :: [Arg (Switch CsplitSwitch)]
argDescs =
    [ mkNamedData Bl "suffix-format" "FORMAT"
          "use sprintf FORMAT instead of %02d"
    , mkNamedData Fl "prefix" "PREFIX" "use PREFIX instead of `xx'"
    , mkNamed     Kl "keep-files" "do not remove output files on errors"
    , mkNamedData Nl "digits" "DIGITS"
          "use specified number of digits instead of 2"
    , mkNamed     Sl "silent" "do not print counts of output file sizes"
    , mkNamed     Zl "elide-empty-files" "remove empty output files"
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "FILE" "FILE"
    ]

-- HELPERS --------------------------------------------------------------------

data Pattern =
    Number Int | RegexCapture Int | RegexSkip Int | Repeat Int | RepeatAll

parsePattern :: String -> Either String Pattern
parsePattern ('/':_) = (Right . RegexCapture) 0
parsePattern ('%':_) = (Right . RegexSkip) 0
parsePattern ('{':y:(ys@(_:_)))
    | isDigit y && last ys == '}' = (Right . Repeat . read . init) ys
    | y == '*' = Right RepeatAll
parsePattern xs'@(x:_) | isDigit x = (Right . Number . read) xs'
parsePattern "" = Left "Empty pattern"
parsePattern xs = Left ("Invalid pattern " ++ xs)

-- Take patterns and lines of text and make the proper text groupings
getSplits :: [Pattern] -> [String] -> [String]
getSplits = go 0
    where go _ _      [] = []
          go _ []     xs = [unlines xs]
          go i (p:ps) xs = maybe
              []
              (\(ni, curr, rest) -> unlines curr : go ni ps rest)
              (getSplit i Nothing p xs)

getSplit :: Int -> Maybe Pattern -> Pattern -> [String]
         -> Maybe (Int, [String], [String])
getSplit _ _ _  [] = Nothing
getSplit i _ (Number n) xs = Just (n, curr, rest)
    where (curr, rest) = splitAt (n - i) xs
getSplit _ _ (Repeat _) _ = Nothing
getSplit _ _ RepeatAll  _ = Nothing
getSplit _ _ _          _ = Nothing

-- CORE -----------------------------------------------------------------------

formatChunks :: Int -> String -> [String] -> [(String, String)]
formatChunks padLevel prefix = zip names
    where names = map ((prefix ++) . padLeft padLevel '0' . show) [0 :: Int ..]

padLeft :: Int -> a -> [a] -> [a]
padLeft n c xs = replicate (n - length xs) c ++ xs

-- FORMAT ---------------------------------------------------------------------

formatSizes :: Maybe [Int] -> String
formatSizes = unlines . map show . fromMaybe []
