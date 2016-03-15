module HusyHox.Common.NumberBase where

import Data.Bits (Bits, (.&.), bitSize, shiftR)

showHexadecimal :: (Bits b, Integral b) => b -> String
showHexadecimal n' = go (bitSize n') n'
    where go 0 _ = ""
          go s n =
              hexChar (15 .&. (fromIntegral n `shiftR` (s - 4))) : go (s - 4) n
          hexChar n | n >= 0  && n < 10 = head $ show n
          hexChar n | n >= 10 && n < 16 = ['a'..'f'] !! (n - 10)
          hexChar n = error ("Out of range: " ++ show n)
