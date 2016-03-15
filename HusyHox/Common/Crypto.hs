module HusyHox.Common.Crypto
    ( CryptoSwitch, IOArgs, CoreArgs, FormatArgs, OutputArgs
    , input, parse, core, format, output, mkArgDescs
    ) where

import Control.Arrow (second)
import Data.ByteString.Lazy.Char8 (ByteString, pack)

import HusyHox.Common.Arg
import HusyHox.Common.Extra (if', readInputsWithPaths)

data CryptoSwitch = Quiet | Status deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [(String, String)]

data CoreArgs = CA (Maybe (Bool, Bool, Bool)) Bool [(String, String)]

data FormatArgs = FA Bool [(String, String)]

data OutputArgs = OA String

input :: Args (Switch CryptoSwitch) -> IO IOArgs
input = fmap IA . readInputsWithPaths . argsRest

-- Kludged with seq until proper error handling is implemented
parse :: Args (Switch CryptoSwitch) -> IOArgs -> CoreArgs
parse args (IA inputs) = seq validateBT $ seq validateV $ CA Nothing binary inputs
    where [binary, text] = map (gotStdArg args) [Bl, Tl]
          validateBT = if'
              ((binary || text) && gotStdArg args Cl)
              (error binaryTextMessage)
              ()
          validateV = map (uncurry (verifyCheckOnlySwitch args))
              [ (Ext Quiet , quietMessage )
              , (Ext Status, statusMessage)
              , (Std Wl    , warnMessage  )
              ]

core :: (Show a) => (ByteString -> a) -> CoreArgs -> FormatArgs
core f (CA _ binary texts) = FA binary $ map (second (show . f . pack)) texts

format :: FormatArgs -> OutputArgs
format (FA binary xs) = OA $ unlines $ map (\(x,y) -> concat [y, " ", b, x]) xs
    where b = if' binary "*" " "

output :: OutputArgs -> IO ()
output (OA text) = putStr text

mkArgDescs :: String -> [Arg (Switch CryptoSwitch)]
mkArgDescs utilName =
    [ mkNamed Bl "binary" "read in binary mode"
    , mkNamed Cl "check"
          ("read " ++ utilName ++ " sums from the FILEs and check them")
    , mkNamed Tl "text"   "read in text mode (default)"
    , mkExt Quiet "quiet"
          "don't print OK for each successfully verified file"
    , mkExt Status "status"
          "don't output anything, status code shows success"
    , mkNamed Wl "warn"
          "warn about improperly formatted checksum lines"
    , mkHelp
    , mkVersion
    ]

-- Helpers --------------------------------------------------------------------

verifyCheckOnlySwitch ::
    Args (Switch CryptoSwitch) ->
    Switch CryptoSwitch ->
    String ->
    ()
verifyCheckOnlySwitch args arg message =
    if' (gotArg args arg && not (gotStdArg args Cl)) (error message) ()

binaryTextMessage, quietMessage, warnMessage, statusMessage :: String

binaryTextMessage =
    "the --binary and --text options are meaningless when verifying checksums"

[quietMessage, warnMessage, statusMessage] = map formatCheckOnlyMessage
    ["quiet", "warn", "status"]

formatCheckOnlyMessage :: String -> String
formatCheckOnlyMessage value = concat
    ["the --", value, " option is meaningful only when verifying checksums"]
