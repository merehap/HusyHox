-- Miscellaneous helper functions

module HusyHox.Common.Extra
    ( readUnnamedArgs, readInputs, readInputsWithPaths
    , readInput, readArgsOrStdin
    , writeOutput, appendOutput, modifyOutput
    , replace, tabSpaces, if', maybeIf, choose
    , atLeastLength, parseIntegral, formatSummary, syntaxError
    , getArgOrDefault, getStdArgOrDefault, selectMatch)
    where

import Control.Arrow (first)
import Data.List (intercalate)
import Data.List.Split (splitOneOf)
import Data.Maybe (fromMaybe)
import System.Console.ParseArgs (Args, argsRest, getArg)

import HusyHox.Common.Arg (StdSwitch, Switch(..))

readUnnamedArgs
    :: (Bounded a, Enum a, Eq a, Ord a, Show a)
    => Args (Switch a) -> IO [String]
readUnnamedArgs = readInputs . argsRest

readInputs :: [String] -> IO [String]
readInputs [] = fmap (:[]) getContents
readInputs xs = mapM (readInput . Just) xs

readInputsWithPaths :: [String] -> IO [(String, String)]
readInputsWithPaths [] = fmap ((:[]) . (,) "-") getContents
readInputsWithPaths xs = mapM (\p -> fmap ((,) p) (readInput $ Just p)) xs

readInput :: Maybe String -> IO String
readInput (Just "" ) = getContents
readInput (Just "-") = getContents
readInput mxs        = maybe getContents readFile mxs

readArgsOrStdin :: [String] -> IO [String]
readArgsOrStdin [] = fmap lines getContents
readArgsOrStdin xs = return xs

writeOutput :: Maybe String -> String -> IO ()
writeOutput = modifyOutput writeFile

appendOutput :: Maybe String -> String -> IO ()
appendOutput = modifyOutput appendFile

modifyOutput :: (String -> String -> IO ()) -> Maybe String -> String -> IO ()
modifyOutput _ Nothing     = putStr
modifyOutput _ (Just "-" ) = putStr
modifyOutput f (Just path) = f path

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOneOf old

tabSpaces :: String
tabSpaces = replicate 8 ' '

if' :: Bool -> a -> a -> a
if' False _ y = y
if' True  x _ = x

maybeIf :: Bool -> (a -> b) -> a -> Maybe b
maybeIf = (.) . choose Just (const Nothing)

choose :: a -> a -> Bool -> a
choose = flip . flip if'

atLeastLength :: Int -> [a] -> Bool
atLeastLength n = ((==n) . length) . take n

parseIntegral :: (Integral i, Read i) => String -> (i, Bool)
parseIntegral ('-':xs) = (read xs, False)
parseIntegral xs       = (read xs, True)

formatSummary :: (Show a, Show b) => [(a,b)] -> String
formatSummary = unlines . map (uncurry formatSummaryLine)

formatSummaryLine :: (Show a, Show b) => a -> b -> String
formatSummaryLine x y = show x ++ " " ++ show y

syntaxError :: a
syntaxError = error "syntax error"

getArgOrDefault :: (Bounded s, Enum s, Ord s, Show s) =>
    String -> Args (Switch s) -> Switch s -> String
getArgOrDefault def args arg = fromMaybe def (getArg args arg)

getStdArgOrDefault :: (Bounded s, Enum s, Ord s, Show s) =>
    String -> Args (Switch s) -> StdSwitch -> String
getStdArgOrDefault def args = getArgOrDefault def args . Std

selectMatch :: b -> (a -> Bool) -> [(a,b)] -> b
selectMatch def selector =
    fromMaybe def . lookup True . map (first selector)
