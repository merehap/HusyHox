module HusyHox.Text.NL (main, nl) where

import HusyHox.Common.Extra (getStdArgOrDefault, if', readInputs)
import HusyHox.Common.Utility

main :: IO ()
main = runUtilityHarness nl

data NLSwitch = DUMMY deriving (Bounded, Enum, Eq, Ord, Show)

data IOArgs = IA [String]

data CoreArgs = CA
    Int      -- line number increment
    [String] -- texts

data FormatArgs = FA [String]

data OutputArgs = OA String

nl :: Utility NLSwitch IOArgs CoreArgs FormatArgs OutputArgs
nl = Utility "nl" argDescs input parse core format output where
    input = fmap IA . readInputs . argsRest

    parse args (IA text) = CA (read $ getStdArgOrDefault "1" args Il) text

    core (CA increment text) =
        FA (numberLines increment $ concatMap lines text)

    format (FA lns) = OA $ unlines lns

    output (OA text) = putStr text

argDescs :: [Arg (Switch NLSwitch)]
argDescs =
    [ mkNamedData Bl "body-numbering" "STYLE"
          "use STYLE for numbering body lines"
    , mkNamedData Dl "section-delimeter" "CC"
          "use CC for separating logical pages"
    , mkNamedData Fl "footer-numbering" "STYLE"
          "use STYLE for numbering footer lines"
    , mkNamedData Hl "header-numbering" "STYLE"
          "use STYLE for numbering header lines"
    , mkNamedData Il "line-increment" "NUMBER"
          "line number increment at each line"
    , mkNamedData Ll "join-blank-lines" "NUMBER"
          "group of NUMBER empty lines counted as one"
    , mkNamedData Nl "number-format" "FORMAT"
          "insert line numbers according to FORMAT"
    , mkNamed Pl "no-renumber"
          "do not reset line numbers at logical pages"
    , mkNamedData Sl "number-separator" "STRING"
          "add STRING after (possible) line number"
    , mkNamedData Vl "starting-line-number" "NUMBER"
          "first line number on each logical page"
    , mkNamedData Wl "number-width" "NUMBER"
          "use NUMBER columns for line numbers"
    , mkHelp
    , mkVersion
    ]

-- Helpers -------------------------------------------------------------------

numberLines :: Int -> [String] -> [String]
numberLines increment = worker 1
    where worker _ []     = []
          worker n (l:ls) = fl : worker nn ls
              where (fl, nn) = if' (null l)
                        ("", n)
                        (formatLine n l, n + increment)

formatLine :: Int -> String -> String
formatLine n = (("     " ++ show n ++ "\t") ++)
