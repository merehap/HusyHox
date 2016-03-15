module HusyHox.Text.Sha224sum (main, sha224sum) where

import Data.Digest.Pure.SHA (sha224)

import HusyHox.Common.Crypto
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness sha224sum

sha224sum :: Utility CryptoSwitch IOArgs CoreArgs FormatArgs OutputArgs
sha224sum = Utility "sha224sum" (mkArgDescs "sha224sum")
    input parse (core sha224) format output
