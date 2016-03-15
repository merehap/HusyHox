module Main where

import qualified Data.Map as M (keys, lookup)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, getProgName)

import HusyHox.Common.Utility (GeneralUtility, runUtility)
import HusyHox.Utils (utils)

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    tryUtil progName args

tryUtil :: String -> [String] -> IO ()
tryUtil progName (name:utilArgs) = do
    _ <- runUtility (getUtil name) progName utilArgs
    return ()
tryUtil _ _ = do
    putStrLn "Please specify one of the following utilities:"
    (putStrLn . unwords) (M.keys utils)

getUtil :: String -> GeneralUtility
getUtil name = fromMaybe
    (error ("Unknown utility '" ++ name ++ "'specified."))
    (M.lookup name utils)
