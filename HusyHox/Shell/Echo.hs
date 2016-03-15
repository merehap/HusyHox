module HusyHox.Shell.Echo (main, echo) where

import HusyHox.Common.Extra (if')
import HusyHox.Common.Format (compressEscapeCharacters)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness echo

data EchoSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA ()

data CoreArgs = CA Bool Bool [String]

data FormatArgs = FA Bool [String]

data OutputArgs = OA String

echo :: Utility EchoSwitch IOArgs CoreArgs FormatArgs OutputArgs
echo = Utility "echo" argDescs input parse core format output where
    input _ = return $ IA ()

    parse args _ = CA escapesEnabled newLine texts
        where texts = argsRest args
              escapesEnabled = gotStdArg args El && not (gotStdArg args E)
              newLine = not $ gotStdArg args Nl

    core (CA escape newline texts) = FA newline (map escapeText texts)
        where escapeText = if' escape compressEscapeCharacters id

    format (FA nl xs) = OA (unwords xs ++ if' nl "\n" "")

    output (OA text) = putStr text

argDescs :: [Arg (Switch EchoSwitch)]
argDescs =
    [ mkAbbrOnly El "enable interpretation of backslash escapes"
    , mkAbbrOnly E
          "disable interpretation of backslash escapes (default)"
    , mkAbbrOnly Nl "do not output the trailing newline"
    ]
