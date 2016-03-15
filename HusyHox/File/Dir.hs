module HusyHox.File.Dir (main, dir) where

import HusyHox.Common.ListDirectoryContents
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness dir

dir :: LdcUtility
dir = ldcUtility "dir"
