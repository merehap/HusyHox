module HusyHox.Text.Sha512sum (main, sha512sum) where

import Data.Digest.Pure.SHA (sha512)

import HusyHox.Common.Crypto
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness sha512sum

sha512sum :: Utility CryptoSwitch IOArgs CoreArgs FormatArgs OutputArgs
sha512sum = Utility "sha512sum" (mkArgDescs "sha512sum")
    input parse (core sha512) format output
