module HusyHox.Text.OD (main, od) where

import Data.Bits (Bits, shiftL)
import Data.List.Split (chunk)
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word16, Word32, Word64)

import HusyHox.Common.Extra (readInputs)
import HusyHox.Common.Utility hiding (One, Two)

main :: IO ()
main = runUtilityHarness od

data ODSwitch = Traditional deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA
    FormatType     -- OutputArgs format
    Radix          -- Printing style for file offsets
    Bool           -- Should output duplicate lines
    Word64         -- Number of bytes to skip
    Word64         -- Number of bytes per output line
    Word64         -- Minimal number of bytes for a graphic char to be output
    (Maybe Word64) -- Maximal number of bytes to read
    [[Word8]]      -- Text inputs

data FormatArgs = FA FormatType Radix Bool [[[WordLike]]]

data OutputArgs = OA String

od :: Utility ODSwitch IOArgs CoreArgs FormatArgs OutputArgs
od = Utility "od" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse args (IA inputs) = CA ftype style dup skip width minIn maxOut w8s
        where ftype  = processFormatType (getArg args $ Std Tl)
              style  = processStyle (getArg args $ Std A)
              dup    = False
              skip   = 0
              width  = 16
              minIn  = 0
              maxOut = Nothing
              w8s    = map (map (fromIntegral . fromEnum)) inputs

    core (CA ftype style dup _ _ _ _ inputs) =
        FA ftype style dup [map (word8sToWordTypes size) inputs]
        where size = formatTypeToSize ftype

    format (FA _ _ _ xs) = OA . unlines $ zipWith label [0 :: Int, 20..] chunks
        where groups :: [String]
              groups = (map (padLeft 6 '0' . show) . concat . concat) xs
              chunks = chunk 8 groups
              label l r = unwords (padLeft 6 '0' (show l) : r)

    output (OA text) = putStrLn text

argDescs :: [Arg (Switch ODSwitch)]
argDescs =
    [ mkNamedData A "address-radix" "RADIX" "decide how file offsets are printed"
    , mkNamedData Jl "skip-bytes" "BYTES" "skip BYTES input bytes first"
    , mkNamedData N  "read-bytes" "BYTES" "limit dump to BYTES input bytes"
    , mkNamedData S  "strings" "BYTES"
          "output strings of at least BYTES graphic chars"
    , mkNamedData Tl "format" "TYPE" "select output format or formats"
    , mkNamed Vl
          "output-duplicates" "do not use * to mark line suppression"
    , mkNamedData Wl "width" "BYTES" "output BYTES bytes per output line"
    , mkExt Traditional "traditional"
          "accept arguments in traditional form"
    , mkAbbrOnly Al
          "same as -t a, select named characters, ignoring high-order list"
    , mkAbbrOnly Bl "same as -t o1, select octal bytes"
    , mkAbbrOnly Cl
          "same as -t c, select ASCII characters or backslash escapes"
    , mkAbbrOnly Dl "same as -t u2, select unsigned decimal 2-byte units"
    , mkAbbrOnly Fl "same as -t fF, select floats"
    , mkAbbrOnly Il "same as -t dI, select decimal ints"
    , mkAbbrOnly Ll "same as -t dL, select decimal longs"
    , mkAbbrOnly Ol "same as -t o2, select octal 2-byte units"
    , mkAbbrOnly Sl "same as -t d2, select decimal 2-byte units"
    , mkAbbrOnly Xl "same as -t x2, select hexadecimal 2-byte units"
    , mkHelp
    , mkVersion
    ]

-- Parse Helpers --------------------------------------------------------------

data FormatTypeParse
    = FT FormatType
    | BadType String
    | BadValue Char

processFormatType :: Maybe String -> FormatType
processFormatType = maybe defaultType getFormatType
    where defaultType = Sized Octal Two
          getFormatType text = case parseFormatType text of
              FT t -> t
              BadType xs -> error ("Invalid option " ++ xs  ++ " for -t.")
              BadValue c -> error ("Invalid size "   ++ [c] ++ " for -t.")

parseFormatType :: String -> FormatTypeParse
parseFormatType xs = case xs of
    ['a']    -> FT (Fixed Named)
    ['c']    -> FT (Fixed Ascii)
    ['d', s] -> makeSized SignedDecimal   s
    ['f', s] -> makeSized FloatingPoint   s
    ['o', s] -> makeSized Octal           s
    ['u', s] -> makeSized UnsignedDecimal s
    ['x', s] -> makeSized Hexadecimal     s
    other    -> BadType other
    where makeSized sizedType size = either 
              BadValue
              (FT . Sized sizedType)
              (charToSize size)

charToSize :: Char -> Either Char Size
charToSize c = case c of
    '1' -> Right One
    '2' -> Right Two
    '4' -> Right Four
    '8' -> Right Eight
    x   -> Left x

processStyle :: Maybe String -> Radix
processStyle = maybe Oct (\v -> fromMaybe (radixError v) $ parseStyle v)
    where radixError v = error ("Invalid radix " ++ v)

parseStyle :: String -> Maybe Radix
parseStyle xs = case xs of
    "n" -> Just None
    "o" -> Just Oct
    "d" -> Just Dec
    "x" -> Just HexDec
    _   -> Nothing

-- Core Helpers ---------------------------------------------------------------

data FormatType = Fixed FixedType | Sized SizedType Size

data FixedType
    = Named
    | Ascii

data SizedType
    = SignedDecimal
    | FloatingPoint
    | Octal
    | UnsignedDecimal
    | Hexadecimal

data Size = One | Two | Four | Eight

data WordLike = W8 Word8 | W16 Word16 | W32 Word32 | W64 Word64

extract :: WordLike -> Integer
extract (W8  w) = toInteger w
extract (W16 w) = toInteger w
extract (W32 w) = toInteger w
extract (W64 w) = toInteger w

instance Show WordLike where
    show = show . extract

data Radix = None | Oct | Dec | HexDec

word8sToWordTypes :: Size -> [Word8] -> [WordLike]
word8sToWordTypes size xs = map wordType ws
    where ws = (map word8sToWord64 . chunk bitCount) padded
          padded = replicate (bitCount * groupCount - len) 0 ++ xs
          wordType = case size of
              One   -> W8  . fromIntegral
              Two   -> W16 . fromIntegral
              Four  -> W32 . fromIntegral
              Eight -> W64 . fromIntegral
          groupCount =
              ceiling (fromIntegral len / fromIntegral bitCount :: Rational)
          len = length xs
          bitCount = sizeToBitCount size

word8sToWord64 :: [Word8] -> Word64
word8sToWord64 =
    sum . zipWith (\s v -> fromIntegral v `shiftL` s) [0,8..] . reverse

sizeToBitCount :: Size -> Int
sizeToBitCount size = case size of
    One   -> 8
    Two   -> 16
    Four  -> 32
    Eight -> 64

formatTypeToSize :: FormatType -> Size
formatTypeToSize (Sized _ size) = size
formatTypeToSize (Fixed _     ) = One

padLeft :: Int -> Char -> String -> String
padLeft n c xs = replicate (n - length xs) c ++ xs
