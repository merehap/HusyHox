module HusyHox.Text.Shasum (main, shasum) where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Digest.Pure.SHA

import HusyHox.Common.Extra (readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness shasum

data IOArgs = IA [String]

data CoreArgs = CA (ByteString -> ByteString) [String]

data FormatArgs = FA [String]

data ShasumSwitch = ShasumSwitch deriving (Bounded, Enum, Eq, Ord, Show)

data OutputArgs = OA String

shasum :: Utility ShasumSwitch IOArgs CoreArgs FormatArgs OutputArgs
shasum = Utility "shasum" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest
    
    parse args (IA text) = CA algo text
        where algo = case getArg args (Std Al) of
                  Nothing    -> s1
                  Just "1"   -> s1
                  Just "224" -> s224
                  Just "256" -> s256
                  Just "384" -> s384
                  Just "512" -> s512
                  Just other -> error ("Unrecognized sha algorithm: " ++ other)

    core (CA algo texts) = FA $ map (show . algo . pack) texts

    format (FA lns) = OA $ unlines lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch ShasumSwitch)]
argDescs =
    [ mkNamedData Al "algorithm" "type" "1 (default), 224, 256, 384, 512"
    , mkHelp
    , mkVersion
    , mkUnnamed Unnamed "FILE"      "FILE"
    ]

-- Helpers --------------------------------------------------------------------

s1, s224, s256, s384, s512 :: ByteString -> ByteString
s1 = bytestringDigest . sha1
s224 = bytestringDigest . sha224
s256 = bytestringDigest . sha256
s384 = bytestringDigest . sha384
s512 = bytestringDigest . sha512
