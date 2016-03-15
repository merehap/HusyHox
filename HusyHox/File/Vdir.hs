module HusyHox.File.Vdir (main, vdir) where

import HusyHox.Common.ListDirectoryContents
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness vdir

vdir :: LdcUtility
vdir = ldcUtility "vdir"
