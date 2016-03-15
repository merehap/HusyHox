module HusyHox.Text.Base64 (main, base64) where

import Codec.Binary.Base64 (decode, encode)
import Data.Char (isAlphaNum)
import Data.List.Split (splitEvery)

import HusyHox.Common.Extra (if', readInput)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness base64

data Base64Switch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA String

data CoreArgs = CA Bool (Maybe Int) String

data FormatArgs = FA (Maybe Int) String

data OutputArgs = OA String

base64 :: Utility Base64Switch IOArgs CoreArgs FormatArgs OutputArgs
base64 = Utility "base64" argDescs input parse core format output where
    input = fmap IA . readInput . flip getStdArg Unnamed

    parse args (IA text) =
        if' (not gar && hasGarbage text)
            (error "invalid input")
            (CA dir (lineLength args)) text
        where [dir, gar] = map (gotStdArg args) [Dl, Il]

    core (CA dir n xs) = FA n (transform xs)
        where transform = if' dir decodeString encodeString

    format (FA Nothing  xs) = OA (xs ++ "\n")
    format (FA (Just n) xs) = OA ((unlines . splitEvery n) xs)

    output (OA text) = putStr text

argDescs :: [Arg (Switch Base64Switch)]
argDescs =
    [ mkNamed     Dl "decode" "decode data"
    , mkNamed     Il "ignore-garbage"
          "when decoding, ignoring non-alphabet characters"
    , mkNamedData Wl "wrap" "COLS"
          ("wrap encoded lines after COLS character (default 76)." ++
           "Use 0 to disable line wrapping")
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "FILE" "FILE"
    ]

-- HELPERS --------------------------------------------------------------------

hasGarbage :: String -> Bool
hasGarbage = not . any isBase64

isBase64 :: Char -> Bool
isBase64 c = elem c "+/" || isAlphaNum c

lineLength :: Args (Switch Base64Switch) -> Maybe Int
lineLength args = if' (len <= 0) Nothing (Just len)
    where len = maybe 76 read $ getStdArg args Wl

encodeString :: String -> String
encodeString = encode . map (fromIntegral . fromEnum)

decodeString :: String -> String
decodeString =
    maybe "" (map (toEnum . fromIntegral)) . decode . filter isBase64
