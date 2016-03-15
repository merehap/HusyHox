module HusyHox.Text.Sha1sum (main, sha1sum) where

import Data.Digest.Pure.SHA (sha1)

import HusyHox.Common.Crypto
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness sha1sum

sha1sum :: Utility CryptoSwitch IOArgs CoreArgs FormatArgs OutputArgs
sha1sum = Utility "sha1sum" (mkArgDescs "sha1sum")
    input parse (core sha1) format output
