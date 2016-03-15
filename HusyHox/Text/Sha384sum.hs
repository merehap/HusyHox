module HusyHox.Text.Sha384sum (main, sha384sum) where

import Data.Digest.Pure.SHA (sha384)

import HusyHox.Common.Crypto
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness sha384sum

sha384sum :: Utility CryptoSwitch IOArgs CoreArgs FormatArgs OutputArgs
sha384sum = Utility "sha384sum" (mkArgDescs "sha384sum")
    input parse (core sha384) format output
