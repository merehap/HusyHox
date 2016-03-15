module HusyHox.Text.Sha256sum (main, sha256sum) where

import Data.Digest.Pure.SHA (sha256)

import HusyHox.Common.Crypto
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness sha256sum

sha256sum :: Utility CryptoSwitch IOArgs CoreArgs FormatArgs OutputArgs
sha256sum = Utility "sha256sum" (mkArgDescs "sha256sum")
    input parse (core sha256) format output
