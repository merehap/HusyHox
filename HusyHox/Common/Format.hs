module HusyHox.Common.Format(getRightIndents, formatRightIndents, fullUnlines,
           compressEscapeCharacters) where

import Data.Char (chr, isHexDigit, isOctDigit)
import Numeric (readHex, readOct)

import Data.List (intercalate)

getRightIndents :: [String] -> [Int]
getRightIndents = map ((+ 1) . length)

formatRightIndents :: [Int] -> [String] -> String
formatRightIndents is xs =
    concat $ zipWith (\i x -> replicate (i - length x) ' ' ++ x) is xs

fullUnlines :: [[String]] -> String
fullUnlines = unlines . map (intercalate "\n")

compressEscapeCharacters :: String -> String
compressEscapeCharacters ""       = ""
compressEscapeCharacters "\\"     = ""
compressEscapeCharacters [x]      = [x]
compressEscapeCharacters (x:y:zs) = cur ++ compressEscapeCharacters rest
    where (cur, rest) = case x of
              '\\' -> case y of
                  '\\'  -> ("\\", zs)
                  'a'   -> (""  , zs)
                  'b'   -> ("\b", zs)
                  'c'   -> (""  , "")
                  'e'   -> ("\b", zs)
                  'f'   -> ("\f", zs)
                  'n'   -> ("\n", zs)
                  'r'   -> ("\r", zs)
                  't'   -> ("\t", zs)
                  'v'   -> ("\v", zs)
                  '0'   -> extract readOct (getOctPrefix zs) zs
                  'x'   -> extract readHex (getHexPrefix zs) zs
                  other -> ([other], zs)
              _    -> ([x], y:zs)

extract :: (String -> [(Int, String)])
        -> (String, Int)
        -> String
        -> (String, String)
extract f (text, len) xs = case f text of
    []  -> ("", "")
    [x] -> ((:[]) . chr $ fst x, drop len xs)
    _   -> error "extract: too many list elements"

getOctPrefix :: String -> (String, Int)
getOctPrefix "" = ("", 0)
getOctPrefix xs = if t < '4' then (text, length text) else ("", 0)
    where text@(t:_) = takeWhile isOctDigit $ take 3 xs

getHexPrefix :: String -> (String, Int)
getHexPrefix xs = (text, length text)
    where text = takeWhile isHexDigit $ take 2 xs
