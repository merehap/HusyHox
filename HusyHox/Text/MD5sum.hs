module HusyHox.Text.MD5sum (main, md5sum) where

import Data.Digest.Pure.MD5 (md5)
import HusyHox.Common.Crypto
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness md5sum

md5sum :: Utility CryptoSwitch IOArgs CoreArgs FormatArgs OutputArgs
md5sum = Utility "md5sum" (mkArgDescs "md5sum")
    input parse (core md5) format output
