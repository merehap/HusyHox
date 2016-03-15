module HusyHox.File.LS (main, ls) where

import HusyHox.Common.ListDirectoryContents
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness ls

ls :: LdcUtility
ls = ldcUtility "ls"
